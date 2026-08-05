{ Secret Orb World Editor - Turbo Vision Edition }
{ Professional TUI-based world editor using Free Pascal Vision }
program EditorTV;

{$MODE OBJFPC}{$H+}

uses
  Objects, Drivers, Views, Menus, App, Dialogs, MsgBox, Editors, SysUtils,
  GameData, DataFile, WorldVal;

const
  VERSION = '0.3.0-TV';

  { Command constants }
  cmNewWorld     = 100;
  cmLoadWorld    = 101;
  cmSaveWorld    = 102;
  cmSaveWorldAs  = 103;
  cmExportBPL    = 104;
  cmExportText   = 105;

  cmListRooms    = 200;
  cmAddRoom      = 201;
  cmEditRoom     = 202;
  cmDeleteRoom   = 203;

  cmListObjects  = 300;
  cmAddObject    = 301;
  cmEditObject   = 302;
  cmDeleteObject = 303;

  cmListMobs     = 400;
  cmAddMob       = 401;
  cmEditMob      = 402;
  cmDeleteMob    = 403;

  cmWorldSettings = 500;
  cmAbout         = 501;
  cmValidate      = 502;

  cmListParas     = 600;
  cmAddPara       = 601;
  cmEditPara      = 602;
  cmDeletePara    = 603;
  cmExportBooklet = 604;
  cmExportXRef    = 605;

  cmListEvents    = 700;
  cmAddEvent      = 701;
  cmEditEvent     = 702;
  cmDeleteEvent   = 703;
  cmEvTrigger     = 704;
  cmEvConds       = 705;
  cmEvActs        = 706;
  cmEvItemAdd     = 707;
  cmEvItemEdit    = 708;
  cmEvItemDel     = 709;

type
  { A modal TDialog ends itself on cmOk, cmCancel, cmYes and cmNo, and on
    nothing else - see TDialog.HandleEvent in the fv package. Every list
    dialog below reports its button through a command of its own, so on a
    plain TDialog the button is drawn, is enabled (a command of 256 or more
    is never disabled) and is pressable, yet does nothing at all: ExecView
    never returns it and the handler behind it is unreachable. This ends the
    modal state for those commands so the code that already tests for them
    runs. }
  PListDialog = ^TListDialog;
  TListDialog = object(TDialog)
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { Main application class }
  TEditorApp = object(TApplication)
    World: TGameWorld;
    CurrentFile: string;
    Modified: Boolean;

    constructor Init;
    procedure InitStatusLine; virtual;
    procedure InitMenuBar; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Idle; virtual;

    { File operations }
    procedure NewWorld;
    procedure LoadWorld;
    procedure SaveWorld;
    procedure SaveWorldAs;
    procedure ExportToBPL;
    procedure ExportToText;

    { Room operations }
    procedure ListRooms;
    procedure AddRoom;
    procedure EditRoomByIndex(Index: Integer);
    procedure DeleteRoom;

    { Object operations }
    procedure ListObjects;
    procedure AddObject;
    procedure EditObjectByIndex(Index: Integer);
    procedure DeleteObject;

    { Mob operations }
    procedure ListMobs;
    procedure AddMob;
    procedure EditMobByIndex(Index: Integer);
    procedure DeleteMob;

    { Story paragraph operations }
    procedure ListParagraphs;
    procedure AddParagraph;
    procedure EditParagraphByNumber(Num: Integer);
    procedure ExportBooklet;
    procedure ExportXRef;

    { Event operations. The full authoring side, unlike editor.pas, which
      ships on the floppy and gets a read-only list. }
    procedure ListEvents;
    procedure AddEvent;
    procedure EditEventBySlot(Slot: Integer);
    procedure EditEventList(var E: TWorldEvent; IsCond: Boolean);
    function EditOneCondition(var C: TCondition): Boolean;
    function EditOneAction(var A: TAction): Boolean;
    function PickIndex(const Title: string; Items: PStringCollection;
                       Preselect: Integer): Integer;

    { World operations }
    procedure WorldSettings;
    procedure ValidateWorldDlg;
    procedure OfferReverseExits(RoomIdx: Integer);
    procedure ShowAbout;
  end;

var
  { Validation scratch. Global because a TIssueList is about 15KB - more than
    belongs on the stack, and nothing here validates reentrantly. }
  Issues: TIssueList;

{ Utility functions }
function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then
    Result := Default;
end;

function BoolToStr(B: Boolean): string;
begin
  if B then
    BoolToStr := 'Yes'
  else
    BoolToStr := 'No';
end;

{ TListDialog Implementation }

procedure TListDialog.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evCommand) and (State and sfModal <> 0) then
    case Event.Command of
      cmEditRoom, cmDeleteRoom,
      cmEditObject, cmDeleteObject,
      cmEditMob, cmDeleteMob,
      cmEditPara, cmDeletePara,
      cmAddEvent, cmEditEvent, cmDeleteEvent,
      cmEvItemAdd, cmEvItemEdit, cmEvItemDel,
      cmEvTrigger, cmEvConds, cmEvActs:
        begin
          EndModal(Event.Command);
          ClearEvent(Event);
        end;
    end;
end;

{ TInputLine.GetData and TInputLine.SetData move a ShortString in and out of
  the record handed to them: GetData zero-fills MaxLen+1 bytes at that address
  and copies the field over the front, SetData copies MaxLen+1 bytes back the
  other way. This unit compiles with long strings on, where a plain `string`
  is an AnsiString - one pointer - so handing one to either call overwrites
  the pointer and the stack behind it, then reads that rubbish back as a
  string.
  Every field in this editor went through those two calls, which is why none
  of them could be typed into and why several OK buttons crashed. Both helpers
  go through a real ShortString instead.

  SetFieldStr truncates to the field's own MaxLen, not to 255. SetData copies
  a fixed MaxLen+1 bytes into a buffer of exactly that size, so a longer value
  would leave a length byte claiming more text than the buffer holds, and the
  next Draw would read off the end of it. }
function GetFieldStr(Field: PInputLine): string;
var
  S: ShortString;
begin
  S := '';
  Field^.GetData(S);
  GetFieldStr := S;
end;

procedure SetFieldStr(Field: PInputLine; const Value: string);
var
  S: ShortString;
begin
  S := Copy(Value, 1, Field^.MaxLen);
  Field^.SetData(S);
end;

{ TEditorApp Implementation }

constructor TEditorApp.Init;
begin
  inherited Init;
  GameData.InitWorld(World);
  CurrentFile := '';
  Modified := False;
end;

procedure TEditorApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F1~ Help', kbF1, cmHelp,
      NewStatusKey('~Alt+X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~F3~ Open', kbF3, cmLoadWorld,
      NewStatusKey('~F2~ Save', kbF2, cmSaveWorld,
      nil)))),
    nil)
  ));
end;

procedure TEditorApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('~N~ew World', 'F4', kbF4, cmNewWorld, hcNoContext,
      NewItem('~O~pen...', 'F3', kbF3, cmLoadWorld, hcNoContext,
      NewItem('~S~ave', 'F2', kbF2, cmSaveWorld, hcNoContext,
      NewItem('S~a~ve As...', '', kbNoKey, cmSaveWorldAs, hcNoContext,
      NewLine(
      NewItem('Export to ~B~PL...', '', kbNoKey, cmExportBPL, hcNoContext,
      NewItem('Export to ~T~ext...', '', kbNoKey, cmExportText, hcNoContext,
      NewLine(
      NewItem('E~x~it', 'Alt+X', kbAltX, cmQuit, hcNoContext,
      nil)))))))))),
    NewSubMenu('~R~ooms', hcNoContext, NewMenu(
      NewItem('~L~ist Rooms', '', kbNoKey, cmListRooms, hcNoContext,
      NewItem('~A~dd Room', '', kbNoKey, cmAddRoom, hcNoContext,
      nil))),
    NewSubMenu('~O~bjects', hcNoContext, NewMenu(
      NewItem('~L~ist Objects', '', kbNoKey, cmListObjects, hcNoContext,
      NewItem('~A~dd Object', '', kbNoKey, cmAddObject, hcNoContext,
      nil))),
    NewSubMenu('~M~obs', hcNoContext, NewMenu(
      NewItem('~L~ist Mobs', '', kbNoKey, cmListMobs, hcNoContext,
      NewItem('~A~dd Mob', '', kbNoKey, cmAddMob, hcNoContext,
      nil))),
    NewSubMenu('~S~tory', hcNoContext, NewMenu(
      NewItem('~L~ist Paragraphs', '', kbNoKey, cmListParas, hcNoContext,
      NewItem('~A~dd Paragraph', '', kbNoKey, cmAddPara, hcNoContext,
      NewLine(
      NewItem('Export ~B~ooklet...', '', kbNoKey, cmExportBooklet, hcNoContext,
      NewItem('Export ~C~ross-reference...', '', kbNoKey, cmExportXRef,
              hcNoContext,
      nil )))))),
    NewSubMenu('~E~vents', hcNoContext, NewMenu(
      NewItem('~L~ist Events', '', kbNoKey, cmListEvents, hcNoContext,
      NewItem('~A~dd Event', '', kbNoKey, cmAddEvent, hcNoContext,
      nil))),
    NewSubMenu('~W~orld', hcNoContext, NewMenu(
      NewItem('~S~ettings...', '', kbNoKey, cmWorldSettings, hcNoContext,
      NewItem('~V~alidate...', '', kbNoKey, cmValidate, hcNoContext,
      NewLine(
      NewItem('~A~bout...', '', kbNoKey, cmAbout, hcNoContext,
      nil))))),
    nil))))))))));
end;

procedure TEditorApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then
  begin
    case Event.Command of
      cmNewWorld:      NewWorld;
      cmLoadWorld:     LoadWorld;
      cmSaveWorld:     SaveWorld;
      cmSaveWorldAs:   SaveWorldAs;
      cmExportBPL:     ExportToBPL;
      cmExportText:    ExportToText;

      cmListRooms:     ListRooms;
      cmAddRoom:       AddRoom;

      cmListObjects:   ListObjects;
      cmAddObject:     AddObject;

      cmListMobs:      ListMobs;
      cmAddMob:        AddMob;

      cmListParas:     ListParagraphs;
      cmAddPara:       AddParagraph;
      cmExportBooklet: ExportBooklet;
      cmExportXRef:    ExportXRef;

      cmListEvents:    ListEvents;
      cmAddEvent:      AddEvent;

      cmWorldSettings: WorldSettings;
      cmValidate:      ValidateWorldDlg;
      cmAbout:         ShowAbout;
    else
      Exit;
    end;
    ClearEvent(Event);
  end;
end;

procedure TEditorApp.Idle;
var
  StatusText: string;
begin
  inherited Idle;

  { Update status line with current file and modified status }
  if CurrentFile <> '' then
    StatusText := ' File: ' + CurrentFile
  else
    StatusText := ' File: (unsaved)';

  if Modified then
    StatusText := StatusText + ' [Modified]';

  { This would update a custom status text if we had one }
end;

{ File Operations }

procedure TEditorApp.NewWorld;
var
  Result: Word;
begin
  if Modified then
  begin
    Result := MessageBox('Discard unsaved changes?', nil,
                         mfWarning + mfYesButton + mfNoButton);
    if Result <> cmYes then
      Exit;
  end;

  GameData.InitWorld(World);
  CurrentFile := '';
  Modified := False;
  MessageBox('New world created.', nil, mfInformation + mfOKButton);
end;

procedure TEditorApp.LoadWorld;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename: string;
begin
  R.Assign(20, 8, 60, 14);
  Dialog := New(PDialog, Init(R, 'Load World'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 37, 3);
    Insert(New(PStaticText, Init(R, 'Filename:')));

    R.Assign(3, 3, 37, 4);
    InputField := New(PInputLine, Init(R, 255));
    Insert(InputField);

    R.Assign(8, 5, 18, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(22, 5, 32, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);

    if Filename <> '' then
    begin
      if DataFile.LoadWorld(Filename, World) then
      begin
        CurrentFile := Filename;
        Modified := False;
        MessageBox('World loaded successfully!', nil, mfInformation + mfOKButton);
      end
      else
        MessageBox('Error loading world file!', nil, mfError + mfOKButton);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.SaveWorld;
var
  Filename: string;
begin
  if CurrentFile = '' then
  begin
    SaveWorldAs;
    Exit;
  end;

  Filename := CurrentFile;
  if DataFile.SaveWorld(Filename, World) then
  begin
    Modified := False;
    MessageBox('World saved successfully!', nil, mfInformation + mfOKButton);
  end
  else
    MessageBox('Error saving world file!', nil, mfError + mfOKButton);
end;

procedure TEditorApp.SaveWorldAs;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename: string;
  DefaultFile: string;
begin
  R.Assign(20, 8, 60, 14);
  Dialog := New(PDialog, Init(R, 'Save World As'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 37, 3);
    Insert(New(PStaticText, Init(R, 'Filename:')));

    R.Assign(3, 3, 37, 4);
    InputField := New(PInputLine, Init(R, 255));
    if CurrentFile <> '' then
      DefaultFile := CurrentFile
    else
      DefaultFile := 'world.dat';
    SetFieldStr(InputField, DefaultFile);
    Insert(InputField);

    R.Assign(8, 5, 18, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(22, 5, 32, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);

    if Filename <> '' then
    begin
      if DataFile.SaveWorld(Filename, World) then
      begin
        CurrentFile := Filename;
        Modified := False;
        MessageBox('World saved successfully!', nil, mfInformation + mfOKButton);
      end
      else
        MessageBox('Error saving world file!', nil, mfError + mfOKButton);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.ExportToBPL;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename: string;
  DefaultFile: string;
begin
  R.Assign(15, 8, 65, 14);
  Dialog := New(PDialog, Init(R, 'Export to BPL'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 47, 3);
    Insert(New(PStaticText, Init(R, 'Filename (BPL format):')));

    R.Assign(3, 3, 47, 4);
    InputField := New(PInputLine, Init(R, 255));
    if CurrentFile <> '' then
      DefaultFile := ChangeFileExt(CurrentFile, '.bpl')
    else
      DefaultFile := 'world.bpl';
    SetFieldStr(InputField, DefaultFile);
    Insert(InputField);

    R.Assign(10, 5, 20, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(25, 5, 35, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);

    if Filename <> '' then
    begin
      if DataFile.SaveWorldAs(Filename, World, sfBPL) then
        MessageBox('Exported to BPL successfully!', nil, mfInformation + mfOKButton)
      else
        MessageBox('Error exporting to BPL!', nil, mfError + mfOKButton);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.ExportToText;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename: string;
  DefaultFile: string;
begin
  R.Assign(15, 8, 65, 14);
  Dialog := New(PDialog, Init(R, 'Export to Text'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 47, 3);
    Insert(New(PStaticText, Init(R, 'Filename (Text format):')));

    R.Assign(3, 3, 47, 4);
    InputField := New(PInputLine, Init(R, 255));
    if CurrentFile <> '' then
      DefaultFile := ChangeFileExt(CurrentFile, '.txt')
    else
      DefaultFile := 'world.txt';
    SetFieldStr(InputField, DefaultFile);
    Insert(InputField);

    R.Assign(10, 5, 20, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(25, 5, 35, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);

    if Filename <> '' then
    begin
      if DataFile.SaveWorldAs(Filename, World, sfText) then
        MessageBox('Exported to text format successfully!', nil, mfInformation + mfOKButton)
      else
        MessageBox('Error exporting to text!', nil, mfError + mfOKButton);
    end;
  end;

  Dispose(Dialog, Done);
end;

{ Room Operations }

procedure TEditorApp.ListRooms;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
  I, Count: Integer;
  Items: PStringCollection;
  ItemStr: string;
  SelectedIndex: Integer;
begin
  { Build list of rooms }
  Items := New(PStringCollection, Init(10, 10));
  Count := 0;

  for I := 1 to MAX_ROOMS do
  begin
    if World.Rooms[I].Active then
    begin
      ItemStr := Format('%3d: %s', [World.Rooms[I].ID, World.Rooms[I].Name]);
      Items^.Insert(NewStr(ItemStr));
      Inc(Count);
    end;
  end;

  if Count = 0 then
  begin
    MessageBox('No rooms defined yet.', nil, mfInformation + mfOKButton);
    Dispose(Items, Done);
    Exit;
  end;

  { Create dialog }
  R.Assign(10, 3, 70, 22);
  Dialog := New(PListDialog, Init(R, 'Room List'));

  with Dialog^ do
  begin
    R.Assign(2, 2, 56, 16);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 55, 16);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(10, 17, 20, 19);
    Insert(New(PButton, Init(R, '~E~dit', cmEditRoom, bfDefault)));

    R.Assign(25, 17, 35, 19);
    Insert(New(PButton, Init(R, '~D~elete', cmDeleteRoom, bfNormal)));

    R.Assign(40, 17, 50, 19);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmEditRoom then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      { Find the actual room index }
      Count := 0;
      for I := 1 to MAX_ROOMS do
      begin
        if World.Rooms[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            EditRoomByIndex(I);
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end
  else if Control = cmDeleteRoom then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      Count := 0;
      for I := 1 to MAX_ROOMS do
      begin
        if World.Rooms[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            if MessageBox('Delete this room?', nil,
                         mfWarning + mfYesButton + mfNoButton) = cmYes then
            begin
              World.Rooms[I].Active := False;
              Modified := True;
              MessageBox('Room deleted.', nil, mfInformation + mfOKButton);
            end;
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.AddRoom;
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField: PInputLine;
  NorthField, SouthField, EastField, WestField, UpField, DownField: PInputLine;
  PointsField, FirstVisitField: PInputLine;
  Control: Word;
  Room: TRoom;
  RoomName, RoomDesc: string;
  NorthStr, SouthStr, EastStr, WestStr, UpStr, DownStr: string;
  PointsStr, FirstVisitStr: string;
  ZeroStr: string;
begin
  ZeroStr := '0';
  if World.RoomCount >= MAX_ROOMS then
  begin
    MessageBox('Maximum number of rooms reached!', nil, mfError + mfOKButton);
    Exit;
  end;

  GameData.InitRoom(Room);
  Room.ID := World.RoomCount + 1;
  Room.Active := True;

  { Create dialog }
  R.Assign(5, 2, 75, 23);
  Dialog := New(PDialog, Init(R, 'Add New Room'));

  with Dialog^ do
  begin
    { Room Name }
    R.Assign(2, 2, 14, 3);
    Insert(New(PStaticText, Init(R, 'Room Name:')));
    R.Assign(15, 2, 65, 3);
    NameField := New(PInputLine, Init(R, MAX_NAME_LEN));
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 14, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(15, 4, 65, 5);
    DescField := New(PInputLine, Init(R, MAX_DESC_LEN));
    Insert(DescField);

    { Exits }
    R.Assign(2, 7, 14, 8);
    Insert(New(PStaticText, Init(R, 'North Exit:')));
    R.Assign(15, 7, 25, 8);
    NorthField := New(PInputLine, Init(R, 5));
    SetFieldStr(NorthField, ZeroStr);
    Insert(NorthField);

    R.Assign(2, 9, 14, 10);
    Insert(New(PStaticText, Init(R, 'South Exit:')));
    R.Assign(15, 9, 25, 10);
    SouthField := New(PInputLine, Init(R, 5));
    SetFieldStr(SouthField, ZeroStr);
    Insert(SouthField);

    R.Assign(2, 11, 14, 12);
    Insert(New(PStaticText, Init(R, 'East Exit:')));
    R.Assign(15, 11, 25, 12);
    EastField := New(PInputLine, Init(R, 5));
    SetFieldStr(EastField, ZeroStr);
    Insert(EastField);

    R.Assign(2, 13, 14, 14);
    Insert(New(PStaticText, Init(R, 'West Exit:')));
    R.Assign(15, 13, 25, 14);
    WestField := New(PInputLine, Init(R, 5));
    SetFieldStr(WestField, ZeroStr);
    Insert(WestField);

    R.Assign(2, 15, 14, 16);
    Insert(New(PStaticText, Init(R, 'Up Exit:')));
    R.Assign(15, 15, 25, 16);
    UpField := New(PInputLine, Init(R, 5));
    SetFieldStr(UpField, ZeroStr);
    Insert(UpField);

    R.Assign(2, 17, 14, 18);
    Insert(New(PStaticText, Init(R, 'Down Exit:')));
    R.Assign(15, 17, 25, 18);
    DownField := New(PInputLine, Init(R, 5));
    SetFieldStr(DownField, ZeroStr);
    Insert(DownField);

    { Score awarded on first visit }
    R.Assign(32, 7, 44, 8);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(45, 7, 55, 8);
    PointsField := New(PInputLine, Init(R, 5));
    SetFieldStr(PointsField, ZeroStr);
    Insert(PointsField);

    R.Assign(32, 9, 66, 10);
    Insert(New(PStaticText, Init(R, 'Scored on first visit only.')));

    { Story paragraph played on first arrival }
    R.Assign(32, 11, 44, 12);
    Insert(New(PStaticText, Init(R, 'First Visit:')));
    R.Assign(45, 11, 55, 12);
    FirstVisitField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstVisitField, ZeroStr);
    Insert(FirstVisitField);

    R.Assign(32, 13, 66, 14);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(20, 19, 30, 21);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(35, 19, 45, 21);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data from fields }
    RoomName := '';
    RoomDesc := '';
    RoomName := GetFieldStr(NameField);
    RoomDesc := GetFieldStr(DescField);

    if RoomName = '' then
    begin
      MessageBox('Room name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Room.Name := RoomName;
    Room.Desc := RoomDesc;

    { Get exit data }
    NorthStr := ''; SouthStr := ''; EastStr := ''; WestStr := ''; UpStr := ''; DownStr := '';
    NorthStr := GetFieldStr(NorthField);
    SouthStr := GetFieldStr(SouthField);
    EastStr := GetFieldStr(EastField);
    WestStr := GetFieldStr(WestField);
    UpStr := GetFieldStr(UpField);
    DownStr := GetFieldStr(DownField);

    Room.Exits[dirNorth] := StrToIntDef(NorthStr, 0);
    Room.Exits[dirSouth] := StrToIntDef(SouthStr, 0);
    Room.Exits[dirEast] := StrToIntDef(EastStr, 0);
    Room.Exits[dirWest] := StrToIntDef(WestStr, 0);
    Room.Exits[dirUp] := StrToIntDef(UpStr, 0);
    Room.Exits[dirDown] := StrToIntDef(DownStr, 0);

    PointsStr := '';
    PointsStr := GetFieldStr(PointsField);
    Room.Points := StrToIntDef(PointsStr, 0);

    FirstVisitStr := '';
    FirstVisitStr := GetFieldStr(FirstVisitField);
    Room.FirstVisitPara := StrToIntDef(FirstVisitStr, 0);

    { Add room to world }
    Inc(World.RoomCount);
    World.Rooms[World.RoomCount] := Room;
    Modified := True;

    MessageBox('Room added successfully!', nil, mfInformation + mfOKButton);
    OfferReverseExits(World.RoomCount);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.EditRoomByIndex(Index: Integer);
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField: PInputLine;
  NorthField, SouthField, EastField, WestField, UpField, DownField: PInputLine;
  PointsField, FirstVisitField: PInputLine;
  Control: Word;
  Room: TRoom;
  RoomName, RoomDesc: string;
  NorthStr, SouthStr, EastStr, WestStr, UpStr, DownStr: string;
  PointsStr, FirstVisitStr: string;
begin
  if (Index < 1) or (Index > MAX_ROOMS) or (not World.Rooms[Index].Active) then
    Exit;

  Room := World.Rooms[Index];

  { Initialize string variables for SetData }
  RoomName := Room.Name;
  RoomDesc := Room.Desc;
  PointsStr := IntToStr(Room.Points);
  FirstVisitStr := IntToStr(Room.FirstVisitPara);
  NorthStr := IntToStr(Room.Exits[dirNorth]);
  SouthStr := IntToStr(Room.Exits[dirSouth]);
  EastStr := IntToStr(Room.Exits[dirEast]);
  WestStr := IntToStr(Room.Exits[dirWest]);
  UpStr := IntToStr(Room.Exits[dirUp]);
  DownStr := IntToStr(Room.Exits[dirDown]);

  { Create dialog }
  R.Assign(5, 2, 75, 23);
  Dialog := New(PDialog, Init(R, 'Edit Room'));

  with Dialog^ do
  begin
    { Room Name }
    R.Assign(2, 2, 14, 3);
    Insert(New(PStaticText, Init(R, 'Room Name:')));
    R.Assign(15, 2, 65, 3);
    NameField := New(PInputLine, Init(R, MAX_NAME_LEN));
    SetFieldStr(NameField, RoomName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 14, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(15, 4, 65, 5);
    DescField := New(PInputLine, Init(R, MAX_DESC_LEN));
    SetFieldStr(DescField, RoomDesc);
    Insert(DescField);

    { Exits }
    R.Assign(2, 7, 14, 8);
    Insert(New(PStaticText, Init(R, 'North Exit:')));
    R.Assign(15, 7, 25, 8);
    NorthField := New(PInputLine, Init(R, 5));
    SetFieldStr(NorthField, NorthStr);
    Insert(NorthField);

    R.Assign(2, 9, 14, 10);
    Insert(New(PStaticText, Init(R, 'South Exit:')));
    R.Assign(15, 9, 25, 10);
    SouthField := New(PInputLine, Init(R, 5));
    SetFieldStr(SouthField, SouthStr);
    Insert(SouthField);

    R.Assign(2, 11, 14, 12);
    Insert(New(PStaticText, Init(R, 'East Exit:')));
    R.Assign(15, 11, 25, 12);
    EastField := New(PInputLine, Init(R, 5));
    SetFieldStr(EastField, EastStr);
    Insert(EastField);

    R.Assign(2, 13, 14, 14);
    Insert(New(PStaticText, Init(R, 'West Exit:')));
    R.Assign(15, 13, 25, 14);
    WestField := New(PInputLine, Init(R, 5));
    SetFieldStr(WestField, WestStr);
    Insert(WestField);

    R.Assign(2, 15, 14, 16);
    Insert(New(PStaticText, Init(R, 'Up Exit:')));
    R.Assign(15, 15, 25, 16);
    UpField := New(PInputLine, Init(R, 5));
    SetFieldStr(UpField, UpStr);
    Insert(UpField);

    R.Assign(2, 17, 14, 18);
    Insert(New(PStaticText, Init(R, 'Down Exit:')));
    R.Assign(15, 17, 25, 18);
    DownField := New(PInputLine, Init(R, 5));
    SetFieldStr(DownField, DownStr);
    Insert(DownField);

    { Score awarded on first visit }
    R.Assign(32, 7, 44, 8);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(45, 7, 55, 8);
    PointsField := New(PInputLine, Init(R, 5));
    SetFieldStr(PointsField, PointsStr);
    Insert(PointsField);

    R.Assign(32, 9, 66, 10);
    Insert(New(PStaticText, Init(R, 'Scored on first visit only.')));

    { Story paragraph played on first arrival }
    R.Assign(32, 11, 44, 12);
    Insert(New(PStaticText, Init(R, 'First Visit:')));
    R.Assign(45, 11, 55, 12);
    FirstVisitField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstVisitField, FirstVisitStr);
    Insert(FirstVisitField);

    R.Assign(32, 13, 66, 14);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(20, 19, 30, 21);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(35, 19, 45, 21);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data from fields }
    RoomName := '';
    RoomDesc := '';
    RoomName := GetFieldStr(NameField);
    RoomDesc := GetFieldStr(DescField);

    if RoomName = '' then
    begin
      MessageBox('Room name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Room.Name := RoomName;
    Room.Desc := RoomDesc;

    { Get exit data }
    NorthStr := ''; SouthStr := ''; EastStr := ''; WestStr := ''; UpStr := ''; DownStr := '';
    NorthStr := GetFieldStr(NorthField);
    SouthStr := GetFieldStr(SouthField);
    EastStr := GetFieldStr(EastField);
    WestStr := GetFieldStr(WestField);
    UpStr := GetFieldStr(UpField);
    DownStr := GetFieldStr(DownField);

    Room.Exits[dirNorth] := StrToIntDef(NorthStr, 0);
    Room.Exits[dirSouth] := StrToIntDef(SouthStr, 0);
    Room.Exits[dirEast] := StrToIntDef(EastStr, 0);
    Room.Exits[dirWest] := StrToIntDef(WestStr, 0);
    Room.Exits[dirUp] := StrToIntDef(UpStr, 0);
    Room.Exits[dirDown] := StrToIntDef(DownStr, 0);

    PointsStr := '';
    PointsStr := GetFieldStr(PointsField);
    Room.Points := StrToIntDef(PointsStr, 0);

    FirstVisitStr := '';
    FirstVisitStr := GetFieldStr(FirstVisitField);
    Room.FirstVisitPara := StrToIntDef(FirstVisitStr, 0);

    { Update room in world }
    World.Rooms[Index] := Room;
    Modified := True;

    MessageBox('Room updated successfully!', nil, mfInformation + mfOKButton);
    OfferReverseExits(Index);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.DeleteRoom;
begin
  ListRooms; { List dialog handles deletion }
end;

{ Object Operations }

procedure TEditorApp.ListObjects;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
  I, Count: Integer;
  Items: PStringCollection;
  ItemStr: string;
  SelectedIndex: Integer;
  FlagStr: string;
begin
  { Build list of objects }
  Items := New(PStringCollection, Init(10, 10));
  Count := 0;

  for I := 1 to MAX_OBJECTS do
  begin
    if World.Objects[I].Active then
    begin
      FlagStr := '';
      if ofPickup in World.Objects[I].Flags then FlagStr := FlagStr + 'P';
      if ofUse in World.Objects[I].Flags then FlagStr := FlagStr + 'U';
      if ofOpen in World.Objects[I].Flags then FlagStr := FlagStr + 'O';
      if ofRead in World.Objects[I].Flags then FlagStr := FlagStr + 'R';

      ItemStr := Format('%3d: %-25s [%s]',
                       [World.Objects[I].ID, World.Objects[I].Name, FlagStr]);
      Items^.Insert(NewStr(ItemStr));
      Inc(Count);
    end;
  end;

  if Count = 0 then
  begin
    MessageBox('No objects defined yet.', nil, mfInformation + mfOKButton);
    Dispose(Items, Done);
    Exit;
  end;

  { Create dialog }
  R.Assign(10, 3, 70, 22);
  Dialog := New(PListDialog, Init(R, 'Object List'));

  with Dialog^ do
  begin
    R.Assign(2, 2, 56, 16);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 55, 16);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(10, 17, 20, 19);
    Insert(New(PButton, Init(R, '~E~dit', cmEditObject, bfDefault)));

    R.Assign(25, 17, 35, 19);
    Insert(New(PButton, Init(R, '~D~elete', cmDeleteObject, bfNormal)));

    R.Assign(40, 17, 50, 19);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmEditObject then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      Count := 0;
      for I := 1 to MAX_OBJECTS do
      begin
        if World.Objects[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            EditObjectByIndex(I);
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end
  else if Control = cmDeleteObject then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      Count := 0;
      for I := 1 to MAX_OBJECTS do
      begin
        if World.Objects[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            if MessageBox('Delete this object?', nil,
                         mfWarning + mfYesButton + mfNoButton) = cmYes then
            begin
              World.Objects[I].Active := False;
              Modified := True;
              MessageBox('Object deleted.', nil, mfInformation + mfOKButton);
            end;
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.AddObject;
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField, RoomIDField, UseTextField: PInputLine;
  PointsField, FirstTakeField: PInputLine;
  PickupCheck: PCheckBoxes;
  Control: Word;
  Obj: TGameObject;
  ObjName, ObjDesc, RoomIDStr, UseTextStr: string;
  PointsStr, FirstTakeStr: string;
  PickupVal: Word;
  ZeroStr: string;
begin
  ZeroStr := '0';
  if World.ObjectCount >= MAX_OBJECTS then
  begin
    MessageBox('Maximum number of objects reached!', nil, mfError + mfOKButton);
    Exit;
  end;

  GameData.InitObject(Obj);
  Obj.ID := World.ObjectCount + 1;
  Obj.Active := True;

  { Create dialog }
  R.Assign(10, 3, 70, 21);
  Dialog := New(PDialog, Init(R, 'Add New Object'));

  with Dialog^ do
  begin
    { Object Name }
    R.Assign(2, 2, 12, 3);
    Insert(New(PStaticText, Init(R, 'Name:')));
    R.Assign(13, 2, 55, 3);
    NameField := New(PInputLine, Init(R, MAX_OBJ_NAME));
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    SetFieldStr(RoomIDField, ZeroStr);
    Insert(RoomIDField);

    { Flags }
    R.Assign(2, 8, 12, 9);
    Insert(New(PStaticText, Init(R, 'Flags:')));

    R.Assign(13, 8, 23, 12);
    PickupCheck := New(PCheckBoxes, Init(R,
      NewSItem('~P~ickup',
      NewSItem('~U~se',
      NewSItem('~O~pen',
      NewSItem('~R~ead',
      nil))))));
    Insert(PickupCheck);

    { Use Text }
    R.Assign(2, 13, 12, 14);
    Insert(New(PStaticText, Init(R, 'Use Text:')));
    R.Assign(13, 13, 55, 14);
    UseTextField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    Insert(UseTextField);

    { Score awarded on first take }
    R.Assign(26, 8, 36, 9);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(37, 8, 47, 9);
    PointsField := New(PInputLine, Init(R, 5));
    SetFieldStr(PointsField, ZeroStr);
    Insert(PointsField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Scored on first take only.')));

    { Story paragraph played on first take }
    R.Assign(26, 12, 36, 13);
    Insert(New(PStaticText, Init(R, 'First Take:')));
    R.Assign(37, 12, 47, 13);
    FirstTakeField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstTakeField, ZeroStr);
    Insert(FirstTakeField);

    R.Assign(26, 13, 56, 14);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(15, 15, 25, 17);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 15, 40, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    ObjName := '';
    ObjDesc := '';
    RoomIDStr := '';
    UseTextStr := '';

    ObjName := GetFieldStr(NameField);
    ObjDesc := GetFieldStr(DescField);
    RoomIDStr := GetFieldStr(RoomIDField);
    UseTextStr := GetFieldStr(UseTextField);

    if ObjName = '' then
    begin
      MessageBox('Object name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Obj.Name := ObjName;
    Obj.Desc := ObjDesc;
    Obj.RoomID := StrToIntDef(RoomIDStr, 0);
    Obj.UseText := UseTextStr;

    { Get flags }
    PickupVal := 0;
    PickupCheck^.GetData(PickupVal);

    Obj.Flags := [];
    if (PickupVal and $01) <> 0 then Include(Obj.Flags, ofPickup);
    if (PickupVal and $02) <> 0 then Include(Obj.Flags, ofUse);
    if (PickupVal and $04) <> 0 then Include(Obj.Flags, ofOpen);
    if (PickupVal and $08) <> 0 then Include(Obj.Flags, ofRead);

    PointsStr := '';
    PointsStr := GetFieldStr(PointsField);
    Obj.Points := StrToIntDef(PointsStr, 0);

    FirstTakeStr := '';
    FirstTakeStr := GetFieldStr(FirstTakeField);
    Obj.FirstTakePara := StrToIntDef(FirstTakeStr, 0);

    { Add to world }
    Inc(World.ObjectCount);
    World.Objects[World.ObjectCount] := Obj;
    Modified := True;

    MessageBox('Object added successfully!', nil, mfInformation + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.EditObjectByIndex(Index: Integer);
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField, RoomIDField, UseTextField: PInputLine;
  PointsField, FirstTakeField: PInputLine;
  PickupCheck: PCheckBoxes;
  Control: Word;
  Obj: TGameObject;
  ObjName, ObjDesc, RoomIDStr, UseTextStr: string;
  PointsStr, FirstTakeStr: string;
  FlagVal: Word;
begin
  if (Index < 1) or (Index > MAX_OBJECTS) or (not World.Objects[Index].Active) then
    Exit;

  Obj := World.Objects[Index];

  { Initialize string variables for SetData }
  ObjName := Obj.Name;
  ObjDesc := Obj.Desc;
  RoomIDStr := IntToStr(Obj.RoomID);
  UseTextStr := Obj.UseText;
  FirstTakeStr := IntToStr(Obj.FirstTakePara);
  PointsStr := IntToStr(Obj.Points);

  { Create dialog }
  R.Assign(10, 3, 70, 21);
  Dialog := New(PDialog, Init(R, 'Edit Object'));

  with Dialog^ do
  begin
    { Object Name }
    R.Assign(2, 2, 12, 3);
    Insert(New(PStaticText, Init(R, 'Name:')));
    R.Assign(13, 2, 55, 3);
    NameField := New(PInputLine, Init(R, MAX_OBJ_NAME));
    SetFieldStr(NameField, ObjName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    SetFieldStr(DescField, ObjDesc);
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    SetFieldStr(RoomIDField, RoomIDStr);
    Insert(RoomIDField);

    { Flags }
    R.Assign(2, 8, 12, 9);
    Insert(New(PStaticText, Init(R, 'Flags:')));

    R.Assign(13, 8, 23, 12);
    PickupCheck := New(PCheckBoxes, Init(R,
      NewSItem('~P~ickup',
      NewSItem('~U~se',
      NewSItem('~O~pen',
      NewSItem('~R~ead',
      nil))))));

    { Set current flag values }
    FlagVal := 0;
    if ofPickup in Obj.Flags then FlagVal := FlagVal or $01;
    if ofUse in Obj.Flags then FlagVal := FlagVal or $02;
    if ofOpen in Obj.Flags then FlagVal := FlagVal or $04;
    if ofRead in Obj.Flags then FlagVal := FlagVal or $08;
    PickupCheck^.SetData(FlagVal);

    Insert(PickupCheck);

    { Use Text }
    R.Assign(2, 13, 12, 14);
    Insert(New(PStaticText, Init(R, 'Use Text:')));
    R.Assign(13, 13, 55, 14);
    UseTextField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    SetFieldStr(UseTextField, UseTextStr);
    Insert(UseTextField);

    { Score awarded on first take }
    R.Assign(26, 8, 36, 9);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(37, 8, 47, 9);
    PointsField := New(PInputLine, Init(R, 5));
    SetFieldStr(PointsField, PointsStr);
    Insert(PointsField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Scored on first take only.')));

    { Story paragraph played on first take }
    R.Assign(26, 12, 36, 13);
    Insert(New(PStaticText, Init(R, 'First Take:')));
    R.Assign(37, 12, 47, 13);
    FirstTakeField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstTakeField, FirstTakeStr);
    Insert(FirstTakeField);

    R.Assign(26, 13, 56, 14);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(15, 15, 25, 17);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 15, 40, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    ObjName := '';
    ObjDesc := '';
    RoomIDStr := '';
    UseTextStr := '';

    ObjName := GetFieldStr(NameField);
    ObjDesc := GetFieldStr(DescField);
    RoomIDStr := GetFieldStr(RoomIDField);
    UseTextStr := GetFieldStr(UseTextField);

    if ObjName = '' then
    begin
      MessageBox('Object name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Obj.Name := ObjName;
    Obj.Desc := ObjDesc;
    Obj.RoomID := StrToIntDef(RoomIDStr, 0);
    Obj.UseText := UseTextStr;

    { Get flags }
    FlagVal := 0;
    PickupCheck^.GetData(FlagVal);

    Obj.Flags := [];
    if (FlagVal and $01) <> 0 then Include(Obj.Flags, ofPickup);
    if (FlagVal and $02) <> 0 then Include(Obj.Flags, ofUse);
    if (FlagVal and $04) <> 0 then Include(Obj.Flags, ofOpen);
    if (FlagVal and $08) <> 0 then Include(Obj.Flags, ofRead);

    PointsStr := '';
    PointsStr := GetFieldStr(PointsField);
    Obj.Points := StrToIntDef(PointsStr, 0);

    FirstTakeStr := '';
    FirstTakeStr := GetFieldStr(FirstTakeField);
    Obj.FirstTakePara := StrToIntDef(FirstTakeStr, 0);

    { Update in world }
    World.Objects[Index] := Obj;
    Modified := True;

    MessageBox('Object updated successfully!', nil, mfInformation + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.DeleteObject;
begin
  ListObjects; { List dialog handles deletion }
end;

{ Mob Operations }

procedure TEditorApp.ListMobs;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
  I, Count: Integer;
  Items: PStringCollection;
  ItemStr: string;
  SelectedIndex: Integer;
begin
  { Build list of mobs }
  Items := New(PStringCollection, Init(10, 10));
  Count := 0;

  for I := 1 to MAX_MOBS do
  begin
    if World.Mobs[I].Active then
    begin
      ItemStr := Format('%3d: %s', [World.Mobs[I].ID, World.Mobs[I].Name]);
      Items^.Insert(NewStr(ItemStr));
      Inc(Count);
    end;
  end;

  if Count = 0 then
  begin
    MessageBox('No mobs defined yet.', nil, mfInformation + mfOKButton);
    Dispose(Items, Done);
    Exit;
  end;

  { Create dialog }
  R.Assign(10, 3, 70, 22);
  Dialog := New(PListDialog, Init(R, 'Mob List'));

  with Dialog^ do
  begin
    R.Assign(2, 2, 56, 16);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 55, 16);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(10, 17, 20, 19);
    Insert(New(PButton, Init(R, '~E~dit', cmEditMob, bfDefault)));

    R.Assign(25, 17, 35, 19);
    Insert(New(PButton, Init(R, '~D~elete', cmDeleteMob, bfNormal)));

    R.Assign(40, 17, 50, 19);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmEditMob then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      Count := 0;
      for I := 1 to MAX_MOBS do
      begin
        if World.Mobs[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            EditMobByIndex(I);
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end
  else if Control = cmDeleteMob then
  begin
    SelectedIndex := ListBox^.Focused;
    if SelectedIndex >= 0 then
    begin
      Count := 0;
      for I := 1 to MAX_MOBS do
      begin
        if World.Mobs[I].Active then
        begin
          if Count = SelectedIndex then
          begin
            if MessageBox('Delete this mob?', nil,
                         mfWarning + mfYesButton + mfNoButton) = cmYes then
            begin
              World.Mobs[I].Active := False;
              Modified := True;
              MessageBox('Mob deleted.', nil, mfInformation + mfOKButton);
            end;
            Break;
          end;
          Inc(Count);
        end;
      end;
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.AddMob;
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField, RoomIDField, DialogueField: PInputLine;
  FirstTalkField: PInputLine;
  Control: Word;
  Mob: TMob;
  MobName, MobDesc, RoomIDStr, DialogueStr: string;
  FirstTalkStr: string;
  ZeroStr: string;
begin
  ZeroStr := '0';
  if World.MobCount >= MAX_MOBS then
  begin
    MessageBox('Maximum number of mobs reached!', nil, mfError + mfOKButton);
    Exit;
  end;

  GameData.InitMob(Mob);
  Mob.ID := World.MobCount + 1;
  Mob.Active := True;

  { Create dialog }
  R.Assign(10, 4, 70, 20);
  Dialog := New(PDialog, Init(R, 'Add New Mob'));

  with Dialog^ do
  begin
    { Mob Name }
    R.Assign(2, 2, 12, 3);
    Insert(New(PStaticText, Init(R, 'Name:')));
    R.Assign(13, 2, 55, 3);
    NameField := New(PInputLine, Init(R, MAX_OBJ_NAME));
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    SetFieldStr(RoomIDField, ZeroStr);
    Insert(RoomIDField);

    { Dialogue }
    R.Assign(2, 8, 12, 9);
    Insert(New(PStaticText, Init(R, 'Dialogue:')));
    R.Assign(13, 8, 55, 9);
    DialogueField := New(PInputLine, Init(R, MAX_DIALOGUE));
    Insert(DialogueField);

    { Story paragraph played the first time the player talks to this mob }
    R.Assign(2, 10, 13, 11);
    Insert(New(PStaticText, Init(R, 'First Talk:')));
    R.Assign(14, 10, 24, 11);
    FirstTalkField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstTalkField, ZeroStr);
    Insert(FirstTalkField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(15, 12, 25, 14);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 12, 40, 14);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    MobName := '';
    MobDesc := '';
    RoomIDStr := '';
    DialogueStr := '';
    FirstTalkStr := '';

    MobName := GetFieldStr(NameField);
    MobDesc := GetFieldStr(DescField);
    RoomIDStr := GetFieldStr(RoomIDField);
    DialogueStr := GetFieldStr(DialogueField);
    FirstTalkStr := GetFieldStr(FirstTalkField);

    if MobName = '' then
    begin
      MessageBox('Mob name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Mob.Name := MobName;
    Mob.Desc := MobDesc;
    Mob.RoomID := StrToIntDef(RoomIDStr, 0);
    Mob.Dialogue := DialogueStr;
    Mob.FirstTalkPara := StrToIntDef(FirstTalkStr, 0);

    { Add to world }
    Inc(World.MobCount);
    World.Mobs[World.MobCount] := Mob;
    Modified := True;

    MessageBox('Mob added successfully!', nil, mfInformation + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.EditMobByIndex(Index: Integer);
var
  Dialog: PDialog;
  R: TRect;
  NameField, DescField, RoomIDField, DialogueField: PInputLine;
  FirstTalkField: PInputLine;
  Control: Word;
  Mob: TMob;
  MobName, MobDesc, RoomIDStr, DialogueStr: string;
  FirstTalkStr: string;
begin
  if (Index < 1) or (Index > MAX_MOBS) or (not World.Mobs[Index].Active) then
    Exit;

  Mob := World.Mobs[Index];

  { Initialize string variables for SetData }
  MobName := Mob.Name;
  MobDesc := Mob.Desc;
  RoomIDStr := IntToStr(Mob.RoomID);
  DialogueStr := Mob.Dialogue;
  FirstTalkStr := IntToStr(Mob.FirstTalkPara);

  { Create dialog }
  R.Assign(10, 4, 70, 20);
  Dialog := New(PDialog, Init(R, 'Edit Mob'));

  with Dialog^ do
  begin
    { Mob Name }
    R.Assign(2, 2, 12, 3);
    Insert(New(PStaticText, Init(R, 'Name:')));
    R.Assign(13, 2, 55, 3);
    NameField := New(PInputLine, Init(R, MAX_OBJ_NAME));
    SetFieldStr(NameField, MobName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    SetFieldStr(DescField, MobDesc);
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    SetFieldStr(RoomIDField, RoomIDStr);
    Insert(RoomIDField);

    { Dialogue }
    R.Assign(2, 8, 12, 9);
    Insert(New(PStaticText, Init(R, 'Dialogue:')));
    R.Assign(13, 8, 55, 9);
    DialogueField := New(PInputLine, Init(R, MAX_DIALOGUE));
    SetFieldStr(DialogueField, DialogueStr);
    Insert(DialogueField);

    { Story paragraph played the first time the player talks to this mob }
    R.Assign(2, 10, 13, 11);
    Insert(New(PStaticText, Init(R, 'First Talk:')));
    R.Assign(14, 10, 24, 11);
    FirstTalkField := New(PInputLine, Init(R, 5));
    SetFieldStr(FirstTalkField, FirstTalkStr);
    Insert(FirstTalkField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Paragraph number, 0 = none.')));

    { Buttons }
    R.Assign(15, 12, 25, 14);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 12, 40, 14);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    MobName := '';
    MobDesc := '';
    RoomIDStr := '';
    DialogueStr := '';
    FirstTalkStr := '';

    MobName := GetFieldStr(NameField);
    MobDesc := GetFieldStr(DescField);
    RoomIDStr := GetFieldStr(RoomIDField);
    DialogueStr := GetFieldStr(DialogueField);
    FirstTalkStr := GetFieldStr(FirstTalkField);

    if MobName = '' then
    begin
      MessageBox('Mob name cannot be empty!', nil, mfError + mfOKButton);
      Dispose(Dialog, Done);
      Exit;
    end;

    Mob.Name := MobName;
    Mob.Desc := MobDesc;
    Mob.RoomID := StrToIntDef(RoomIDStr, 0);
    Mob.Dialogue := DialogueStr;
    Mob.FirstTalkPara := StrToIntDef(FirstTalkStr, 0);

    { Update in world }
    World.Mobs[Index] := Mob;
    Modified := True;

    MessageBox('Mob updated successfully!', nil, mfInformation + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.DeleteMob;
begin
  ListMobs; { List dialog handles deletion }
end;

{ World Operations }

{ Story Paragraph Operations }

{ Collapses the memo's line endings to #13#10 so every editor and every
  export format sees the same breaks }
function NormaliseBreaks(const S: TParaText): TParaText;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = #13) or (S[I] = #10) then
    begin
      Result := Result + #13#10;
      if (S[I] = #13) and (I < Length(S)) and (S[I + 1] = #10) then Inc(I);
    end
    else
      Result := Result + S[I];
    Inc(I);
  end;
end;

function ParaPreview(const S: TParaText): string;
var
  I: Integer;
  T: string;
begin
  T := Copy(S, 1, 58);
  for I := 1 to Length(T) do
    if (T[I] = #13) or (T[I] = #10) then T[I] := ' ';
  ParaPreview := T;
end;

procedure TEditorApp.EditParagraphByNumber(Num: Integer);
type
  { Matches TMemo.DataSize, which is BufSize + SizeOf(Sw_Word) }
  TParaMemoRec = record
    Length: Sw_Word;
    Buffer: array[0..MAX_PARA_LEN - 1] of Char;
  end;
var
  Dialog: PDialog;
  R: TRect;
  Memo: PMemo;
  VScroll: PScrollBar;
  Control: Word;
  Data: TParaMemoRec;
  Existing: TParaText;
  Len: Integer;
begin
  if (Num < 1) or (Num > MAX_PARAGRAPHS) then Exit;

  Existing := World.Paragraphs[Num];
  Len := Length(Existing);
  if Len > MAX_PARA_LEN then Len := MAX_PARA_LEN;
  FillChar(Data, SizeOf(Data), 0);
  Data.Length := Len;
  if Len > 0 then
    Move(Existing[1], Data.Buffer[0], Len);

  R.Assign(8, 3, 72, 22);
  Dialog := New(PDialog, Init(R, 'Paragraph ' + IntToStr(Num)));

  with Dialog^ do
  begin
    R.Assign(60, 2, 61, 14);
    VScroll := New(PScrollBar, Init(R));
    Insert(VScroll);

    R.Assign(2, 2, 60, 14);
    Memo := New(PMemo, Init(R, nil, VScroll, nil, MAX_PARA_LEN));
    Memo^.SetData(Data);
    Insert(Memo);

    R.Assign(2, 14, 60, 15);
    Insert(New(PStaticText, Init(R,
      'This number is printed in the booklet. Blank lines are kept.')));

    R.Assign(16, 15, 26, 17);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(32, 15, 42, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    FillChar(Data, SizeOf(Data), 0);
    Memo^.GetData(Data);
    Len := Data.Length;
    if Len > MAX_PARA_LEN then Len := MAX_PARA_LEN;
    SetLength(Existing, Len);
    if Len > 0 then
      Move(Data.Buffer[0], Existing[1], Len);
    SetParagraph(World, Num, NormaliseBreaks(Existing));
    Modified := True;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.AddParagraph;
var
  Dialog: PDialog;
  R: TRect;
  NumField: PInputLine;
  Control: Word;
  NumStr: string;
  Num, I: Integer;
begin
  { Suggest the first free slot, but let the author choose the number }
  Num := 1;
  for I := 1 to MAX_PARAGRAPHS do
    if World.Paragraphs[I] = '' then
    begin
      Num := I;
      Break;
    end;
  NumStr := IntToStr(Num);

  R.Assign(20, 8, 60, 15);
  Dialog := New(PDialog, Init(R, 'Add Paragraph'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 37, 3);
    Insert(New(PStaticText, Init(R,
      'Paragraph number (1-' + IntToStr(MAX_PARAGRAPHS) + '):')));

    R.Assign(3, 3, 13, 4);
    NumField := New(PInputLine, Init(R, 5));
    SetFieldStr(NumField, NumStr);
    Insert(NumField);

    R.Assign(3, 4, 37, 5);
    Insert(New(PStaticText, Init(R, 'An existing number is edited.')));

    R.Assign(6, 5, 16, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(21, 5, 31, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    NumStr := '';
    NumStr := GetFieldStr(NumField);
    Num := StrToIntDef(NumStr, 0);
    if (Num >= 1) and (Num <= MAX_PARAGRAPHS) then
    begin
      Dispose(Dialog, Done);
      EditParagraphByNumber(Num);
      Exit;
    end
    else
      MessageBox('Paragraph number out of range.', nil, mfError + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.ListParagraphs;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
  I, Count: Integer;
  Items: PStringCollection;
  Numbers: array[1..MAX_PARAGRAPHS] of Integer;
  SelectedIndex: Integer;
begin
  Items := New(PStringCollection, Init(10, 10));
  Count := 0;

  for I := 1 to MAX_PARAGRAPHS do
    if World.Paragraphs[I] <> '' then
    begin
      Inc(Count);
      Numbers[Count] := I;
      Items^.Insert(NewStr(Format('%3d: %s', [I, ParaPreview(World.Paragraphs[I])])));
    end;

  if Count = 0 then
  begin
    MessageBox('No paragraphs yet. Use Story / Add Paragraph.', nil,
               mfInformation + mfOKButton);
    Dispose(Items, Done);
    Exit;
  end;

  R.Assign(6, 3, 74, 22);
  Dialog := New(PListDialog, Init(R, 'Story Paragraphs'));

  with Dialog^ do
  begin
    R.Assign(64, 2, 65, 16);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 64, 16);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(10, 16, 20, 18);
    Insert(New(PButton, Init(R, '~E~dit', cmEditPara, bfDefault)));

    R.Assign(25, 16, 35, 18);
    Insert(New(PButton, Init(R, '~D~elete', cmDeletePara, bfNormal)));

    R.Assign(45, 16, 55, 18);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);
  SelectedIndex := ListBox^.Focused;

  if (Control = cmEditPara) and (SelectedIndex >= 0) and
     (SelectedIndex < Count) then
  begin
    Dispose(Dialog, Done);
    EditParagraphByNumber(Numbers[SelectedIndex + 1]);
    Exit;
  end
  else if (Control = cmDeletePara) and (SelectedIndex >= 0) and
          (SelectedIndex < Count) then
  begin
    if MessageBox('Delete this paragraph? The number stays reserved, ' +
                  'so a printed booklet keeps matching.', nil,
                  mfWarning + mfYesButton + mfNoButton) = cmYes then
    begin
      SetParagraph(World, Numbers[SelectedIndex + 1], '');
      Modified := True;
    end;
  end;

  Dispose(Dialog, Done);
end;

{ The author's companion to the booklet: what fires each paragraph. A separate
  file, because the booklet itself goes to the player. }
procedure TEditorApp.ExportXRef;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename, DefaultFile: string;
begin
  R.Assign(15, 8, 65, 14);
  Dialog := New(PDialog, Init(R, 'Export Cross-reference'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 47, 3);
    Insert(New(PStaticText, Init(R, 'Cross-reference filename:')));

    R.Assign(3, 3, 47, 4);
    InputField := New(PInputLine, Init(R, 255));
    DefaultFile := 'ORBXREF.TXT';
    SetFieldStr(InputField, DefaultFile);
    Insert(InputField);

    R.Assign(10, 5, 20, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(25, 5, 35, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);
    if Filename <> '' then
    begin
      if WriteParaXRef(Filename, World) then
        MessageBox('Cross-reference written.', nil,
                   mfInformation + mfOKButton)
      else
        MessageBox('Error writing cross-reference!', nil, mfError + mfOKButton);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TEditorApp.ExportBooklet;
var
  Dialog: PDialog;
  R: TRect;
  InputField: PInputLine;
  Control: Word;
  Filename, DefaultFile: string;
  F: Text;
  I, Written: Integer;
begin
  R.Assign(15, 8, 65, 14);
  Dialog := New(PDialog, Init(R, 'Export Booklet'));

  with Dialog^ do
  begin
    R.Assign(3, 2, 47, 3);
    Insert(New(PStaticText, Init(R, 'Printable booklet filename:')));

    R.Assign(3, 3, 47, 4);
    InputField := New(PInputLine, Init(R, 255));
    DefaultFile := 'ORBLORE.TXT';
    SetFieldStr(InputField, DefaultFile);
    Insert(InputField);

    R.Assign(10, 5, 20, 7);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(25, 5, 35, 7);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    Filename := '';
    Filename := GetFieldStr(InputField);

    if Filename <> '' then
    begin
      {$I-}
      Assign(F, Filename);
      Rewrite(F);
      {$I+}
      if IOResult <> 0 then
        MessageBox('Could not write that file.', nil, mfError + mfOKButton)
      else
      begin
        WriteLn(F, World.Title);
        WriteLn(F, StringOfChar('=', Length(World.Title)));
        WriteLn(F);
        WriteLn(F, 'Do not read ahead. Read each paragraph only when the');
        WriteLn(F, 'game tells you to.');
        WriteLn(F);

        Written := 0;
        for I := 1 to World.ParaCount do
          if World.Paragraphs[I] <> '' then
          begin
            WriteLn(F, '--- ', I, ' ---');
            WriteParaBody(F, World.Paragraphs[I]);
            WriteLn(F);
            Inc(Written);
          end;

        Close(F);
        MessageBox(Format('Wrote %d paragraphs to the booklet.', [Written]),
                   nil, mfInformation + mfOKButton);
      end;
    end;
  end;

  Dispose(Dialog, Done);
end;

{ ---- Events -------------------------------------------------------------

  The full authoring side lives here and in the browser editor, because
  neither has a size budget - editor.pas ships on the floppy beside the game
  and gets a read-only list. What all this code is really for is keeping an
  author from having to know the encodings: which of a trigger's two IDs
  means what, and that a locked exit packs a direction and a destination into
  one number. The labels come from the type in hand, so the dialog asks for
  "Object ID" or "Flag number" rather than "Target". }

{ The human half of the pick lists. The enum spellings are file format and
  must not change; these can. }
{ Format has no conditional, and an if-else around every row would bury what
  the row says. }
function BoolText(B: Boolean; const WhenTrue, WhenFalse: string): string;
begin
  if B then BoolText := WhenTrue else BoolText := WhenFalse;
end;

function TriggerHelp(T: TEventTrigger): string;
begin
  case T of
    etEnterRoom:     TriggerHelp := 'player enters a room';
    etExitRoom:      TriggerHelp := 'player leaves a room';
    etFirstVisit:    TriggerHelp := 'player''s first visit to a room';
    etTakeObject:    TriggerHelp := 'player takes an object';
    etDropObject:    TriggerHelp := 'player drops an object';
    etUseObject:     TriggerHelp := 'player uses an object';
    etUseObjectOn:   TriggerHelp := 'player uses one thing on another';
    etExamineObject: TriggerHelp := 'player examines an object';
    etTalkToMob:     TriggerHelp := 'player talks to a mob';
    etGiveTo:        TriggerHelp := 'player gives something to a mob';
    etTimer:         TriggerHelp := 'a number of turns have passed';
    etFlagSet:       TriggerHelp := 'a flag becomes set';
    etFlagClear:     TriggerHelp := 'a flag becomes clear';
  else
    TriggerHelp := '';
  end;
end;

{ What a trigger's first and second ID mean. An empty second label means the
  engine always passes 0 there, so the field is hidden - filling it in would
  make the event dead, which is what the validator reports. }
function TriggerLabel1(T: TEventTrigger): string;
begin
  case T of
    etEnterRoom:     TriggerLabel1 := 'Room entered:';
    etExitRoom:      TriggerLabel1 := 'Room left:';
    etFirstVisit:    TriggerLabel1 := 'Room:';
    etTakeObject:    TriggerLabel1 := 'Object taken:';
    etDropObject:    TriggerLabel1 := 'Object dropped:';
    etUseObject:     TriggerLabel1 := 'Object used:';
    etUseObjectOn:   TriggerLabel1 := 'Object used:';
    etExamineObject: TriggerLabel1 := 'Object examined:';
    etTalkToMob:     TriggerLabel1 := 'Mob:';
    etGiveTo:        TriggerLabel1 := 'Object given:';
    etTimer:         TriggerLabel1 := 'Fires on turn:';
    etFlagSet:       TriggerLabel1 := 'Flag set:';
    etFlagClear:     TriggerLabel1 := 'Flag cleared:';
  else
    TriggerLabel1 := 'ID:';
  end;
end;

function TriggerLabel2(T: TEventTrigger): string;
begin
  case T of
    etExitRoom:    TriggerLabel2 := 'Room entered:';
    etUseObjectOn: TriggerLabel2 := 'Used on object:';
    etGiveTo:      TriggerLabel2 := 'Given to mob:';
    etTimer:       TriggerLabel2 := 'Then every:';
  else
    TriggerLabel2 := '';
  end;
end;

function CondHelp(C: TConditionType): string;
begin
  case C of
    ctNone:           CondHelp := 'nothing, always passes';
    ctHasObject:      CondHelp := 'player is carrying an object';
    ctObjectInRoom:   CondHelp := 'an object is in a room';
    ctMobInRoom:      CondHelp := 'a mob is in a room';
    ctFlagIsSet:      CondHelp := 'a flag is set';
    ctFlagIsClear:    CondHelp := 'a flag is clear';
    ctCounterEquals:  CondHelp := 'a counter equals a number';
    ctCounterGreater: CondHelp := 'a counter is more than a number';
    ctCounterLess:    CondHelp := 'a counter is less than a number';
    ctVisitedRoom:    CondHelp := 'the player has visited a room';
    ctRoomIs:         CondHelp := 'the player is in a room';
  else
    CondHelp := '';
  end;
end;

function CondLabel1(C: TConditionType): string;
begin
  case C of
    ctHasObject, ctObjectInRoom: CondLabel1 := 'Object ID:';
    ctMobInRoom:                 CondLabel1 := 'Mob ID:';
    ctFlagIsSet, ctFlagIsClear:  CondLabel1 := 'Flag number:';
    ctCounterEquals, ctCounterGreater,
    ctCounterLess:               CondLabel1 := 'Counter number:';
    ctVisitedRoom, ctRoomIs:     CondLabel1 := 'Room ID:';
  else
    CondLabel1 := '';
  end;
end;

function CondLabel2(C: TConditionType): string;
begin
  case C of
    ctObjectInRoom, ctMobInRoom: CondLabel2 := 'In room ID:';
    ctCounterEquals, ctCounterGreater,
    ctCounterLess:               CondLabel2 := 'Compared with:';
  else
    CondLabel2 := '';
  end;
end;

function ActHelp(A: TActionType): string;
begin
  case A of
    atNone:           ActHelp := 'nothing';
    atShowMessage:    ActHelp := 'show a one-line message';
    atShowParagraph:  ActHelp := 'show a story paragraph';
    atSetFlag:        ActHelp := 'set a flag';
    atClearFlag:      ActHelp := 'clear a flag';
    atToggleFlag:     ActHelp := 'toggle a flag';
    atSetCounter:     ActHelp := 'set a counter to a number';
    atAddCounter:     ActHelp := 'add to a counter';
    atSubCounter:     ActHelp := 'subtract from a counter';
    atMoveObject:     ActHelp := 'move an object to a room';
    atRemoveObject:   ActHelp := 'take an object out of play';
    atSpawnObject:    ActHelp := 'place an object in a room';
    atMoveMob:        ActHelp := 'move a mob to a room';
    atRemoveMob:      ActHelp := 'take a mob out of play';
    atUnlockExit:     ActHelp := 'open an exit';
    atLockExit:       ActHelp := 'shut an exit';
    atTeleportPlayer: ActHelp := 'move the player to a room';
    atAddScore:       ActHelp := 'award points';
    atEndGame:        ActHelp := 'end the game';
    atEnableEvent:    ActHelp := 'enable another event';
    atDisableEvent:   ActHelp := 'disable another event';
  else
    ActHelp := '';
  end;
end;

function ActLabel1(A: TActionType): string;
begin
  case A of
    atShowParagraph:  ActLabel1 := 'Paragraph number:';
    atSetFlag, atClearFlag, atToggleFlag:
                      ActLabel1 := 'Flag number:';
    atSetCounter, atAddCounter, atSubCounter:
                      ActLabel1 := 'Counter number:';
    atMoveObject, atSpawnObject, atRemoveObject:
                      ActLabel1 := 'Object ID:';
    atMoveMob, atRemoveMob:
                      ActLabel1 := 'Mob ID:';
    atLockExit, atUnlockExit, atTeleportPlayer:
                      ActLabel1 := 'Room ID:';
    atAddScore:       ActLabel1 := 'Points:';
    atEnableEvent, atDisableEvent:
                      ActLabel1 := 'Event slot:';
  else
    ActLabel1 := '';
  end;
end;

function ActLabel2(A: TActionType): string;
begin
  case A of
    atSetCounter, atAddCounter, atSubCounter:
                      ActLabel2 := 'Amount:';
    atMoveObject, atSpawnObject, atMoveMob:
                      ActLabel2 := 'Destination room:';
    atEndGame:        ActLabel2 := 'Ending (0 win, 1 lose):';
  else
    ActLabel2 := '';
  end;
end;

{ A modal list picker, used for the three enums. NewList takes ownership of
  Items, so the dialog disposes them either way. Returns the chosen index or
  -1 if the author cancelled. }
function TEditorApp.PickIndex(const Title: string; Items: PStringCollection;
                              Preselect: Integer): Integer;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
begin
  PickIndex := -1;
  R.Assign(12, 2, 68, 22);
  Dialog := New(PDialog, Init(R, Title));

  with Dialog^ do
  begin
    R.Assign(52, 2, 53, 15);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 52, 15);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    if (Preselect >= 0) and (Preselect < Items^.Count) then
      ListBox^.FocusItem(Preselect);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(12, 16, 22, 18);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
    R.Assign(28, 16, 40, 18);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);
  if Control = cmOK then PickIndex := ListBox^.Focused;
  Dispose(Dialog, Done);
end;

{ Type first, then the numbers - the dialog cannot relabel itself, so it is
  built after the type is known and asks only for the fields that type uses. }
function TEditorApp.EditOneCondition(var C: TCondition): Boolean;
var
  Items: PStringCollection;
  CT: TConditionType;
  Idx, Y: Integer;
  Dialog: PDialog;
  R: TRect;
  Field1, Field2: PInputLine;
  NegBox: PCheckBoxes;
  Control, Flags: Word;
  S: string;
begin
  EditOneCondition := False;

  Items := New(PStringCollection, Init(12, 4));
  for CT := Low(TConditionType) to High(TConditionType) do
    Items^.Insert(NewStr(ConditionName(CT) + ' - ' + CondHelp(CT)));
  Idx := PickIndex('Condition type', Items, Ord(C.CondType));
  if Idx < 0 then Exit;
  C.CondType := TConditionType(Idx);

  if C.CondType = ctNone then
  begin
    C.TargetID := 0;
    C.Value := 0;
    C.Negate := False;
    EditOneCondition := True;
    Exit;
  end;

  R.Assign(14, 6, 66, 18);
  Dialog := New(PDialog, Init(R, 'Condition: ' + ConditionName(C.CondType)));
  Field1 := nil;
  Field2 := nil;
  Y := 2;

  with Dialog^ do
  begin
    R.Assign(2, Y, 50, Y + 1);
    Insert(New(PStaticText, Init(R, CondHelp(C.CondType))));
    Inc(Y, 2);

    if CondLabel1(C.CondType) <> '' then
    begin
      R.Assign(2, Y, 22, Y + 1);
      Insert(New(PStaticText, Init(R, CondLabel1(C.CondType))));
      R.Assign(23, Y, 33, Y + 1);
      Field1 := New(PInputLine, Init(R, 6));
      S := IntToStr(C.TargetID);
      SetFieldStr(Field1, S);
      Insert(Field1);
      Inc(Y, 2);
    end;

    if CondLabel2(C.CondType) <> '' then
    begin
      R.Assign(2, Y, 22, Y + 1);
      Insert(New(PStaticText, Init(R, CondLabel2(C.CondType))));
      R.Assign(23, Y, 33, Y + 1);
      Field2 := New(PInputLine, Init(R, 6));
      S := IntToStr(C.Value);
      SetFieldStr(Field2, S);
      Insert(Field2);
      Inc(Y, 2);
    end;

    R.Assign(2, Y, 48, Y + 1);
    NegBox := New(PCheckBoxes, Init(R,
      NewSItem('~N~OT - the condition must be false', nil)));
    if C.Negate then Flags := 1 else Flags := 0;
    NegBox^.SetData(Flags);
    Insert(NegBox);

    R.Assign(10, 9, 20, 11);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
    R.Assign(26, 9, 38, 11);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);
  if Control = cmOK then
  begin
    if Field1 <> nil then
    begin
      S := '';
      S := GetFieldStr(Field1);
      C.TargetID := StrToIntDef(S, 0);
    end
    else
      C.TargetID := 0;

    if Field2 <> nil then
    begin
      S := '';
      S := GetFieldStr(Field2);
      C.Value := StrToIntDef(S, 0);
    end
    else
      C.Value := 0;

    NegBox^.GetData(Flags);
    C.Negate := (Flags and 1) <> 0;
    EditOneCondition := True;
  end;

  Dispose(Dialog, Done);
end;

function TEditorApp.EditOneAction(var A: TAction): Boolean;
var
  Items: PStringCollection;
  AT: TActionType;
  Idx, Y: Integer;
  Dialog: PDialog;
  R: TRect;
  Field1, Field2, TextField: PInputLine;
  DirBox: PRadioButtons;
  Control, Flags: Word;
  S: string;
  Dir: TDirection;
  Dest: Word;
begin
  EditOneAction := False;

  Items := New(PStringCollection, Init(24, 4));
  for AT := Low(TActionType) to High(TActionType) do
    Items^.Insert(NewStr(ActionName(AT) + ' - ' + ActHelp(AT)));
  Idx := PickIndex('Action', Items, Ord(A.ActionType));
  if Idx < 0 then Exit;
  A.ActionType := TActionType(Idx);

  if A.ActionType = atNone then
  begin
    A.TargetID := 0;
    A.Value := 0;
    A.Text := '';
    EditOneAction := True;
    Exit;
  end;

  R.Assign(10, 4, 70, 21);
  Dialog := New(PDialog, Init(R, 'Action: ' + ActionName(A.ActionType)));
  Field1 := nil;
  Field2 := nil;
  TextField := nil;
  DirBox := nil;
  Y := 2;

  with Dialog^ do
  begin
    R.Assign(2, Y, 58, Y + 1);
    Insert(New(PStaticText, Init(R, ActHelp(A.ActionType))));
    Inc(Y, 2);

    if ActLabel1(A.ActionType) <> '' then
    begin
      R.Assign(2, Y, 24, Y + 1);
      Insert(New(PStaticText, Init(R, ActLabel1(A.ActionType))));
      R.Assign(25, Y, 35, Y + 1);
      Field1 := New(PInputLine, Init(R, 6));
      S := IntToStr(A.TargetID);
      SetFieldStr(Field1, S);
      Insert(Field1);
      Inc(Y, 2);
    end;

    { The exit actions are the reason this dialog is built by hand: the
      direction and the destination share one number, the direction in the
      low three bits. Locking discards the destination, which is why
      unlocking has to name it again. }
    if A.ActionType in [atLockExit, atUnlockExit] then
    begin
      DecodeExitValue(A.Value, Dir, Dest);
      R.Assign(2, Y, 24, Y + 1);
      Insert(New(PStaticText, Init(R, 'Direction:')));
      R.Assign(25, Y, 45, Y + 6);
      DirBox := New(PRadioButtons, Init(R,
        NewSItem('~N~orth', NewSItem('~S~outh', NewSItem('~E~ast',
        NewSItem('~W~est', NewSItem('~U~p', NewSItem('~D~own', nil))))))));
      Flags := Ord(Dir);
      DirBox^.SetData(Flags);
      Insert(DirBox);
      Inc(Y, 6);

      if A.ActionType = atUnlockExit then
      begin
        R.Assign(2, Y, 24, Y + 1);
        Insert(New(PStaticText, Init(R, 'Leads to room:')));
        R.Assign(25, Y, 35, Y + 1);
        Field2 := New(PInputLine, Init(R, 6));
        S := IntToStr(Dest);
        SetFieldStr(Field2, S);
        Insert(Field2);
        Inc(Y, 2);
      end;
    end
    else if ActLabel2(A.ActionType) <> '' then
    begin
      R.Assign(2, Y, 24, Y + 1);
      Insert(New(PStaticText, Init(R, ActLabel2(A.ActionType))));
      R.Assign(25, Y, 35, Y + 1);
      Field2 := New(PInputLine, Init(R, 6));
      S := IntToStr(A.Value);
      SetFieldStr(Field2, S);
      Insert(Field2);
      Inc(Y, 2);
    end;

    if A.ActionType = atShowMessage then
    begin
      R.Assign(2, Y, 24, Y + 1);
      Insert(New(PStaticText, Init(R, 'Message:')));
      Inc(Y);
      R.Assign(2, Y, 58, Y + 1);
      TextField := New(PInputLine, Init(R, MAX_EVENT_TEXT));
      S := A.Text;
      SetFieldStr(TextField, S);
      Insert(TextField);
      Inc(Y, 2);
      R.Assign(2, Y, 58, Y + 1);
      Insert(New(PStaticText, Init(R,
        'One line. Longer prose belongs in a paragraph.')));
    end;

    R.Assign(12, 14, 22, 16);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
    R.Assign(30, 14, 42, 16);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);
  if Control = cmOK then
  begin
    if Field1 <> nil then
    begin
      S := '';
      S := GetFieldStr(Field1);
      A.TargetID := StrToIntDef(S, 0);
    end
    else
      A.TargetID := 0;

    if DirBox <> nil then
    begin
      DirBox^.GetData(Flags);
      if Flags > Ord(High(TDirection)) then Flags := 0;
      Dest := 0;
      if Field2 <> nil then
      begin
        S := '';
        S := GetFieldStr(Field2);
        Dest := StrToIntDef(S, 0);
      end;
      A.Value := EncodeExitValue(TDirection(Flags), Dest);
    end
    else if Field2 <> nil then
    begin
      S := '';
      S := GetFieldStr(Field2);
      A.Value := StrToIntDef(S, 0);
    end
    else
      A.Value := 0;

    if TextField <> nil then
    begin
      S := '';
      S := GetFieldStr(TextField);
      A.Text := S;
    end
    else
      A.Text := '';

    EditOneAction := True;
  end;

  Dispose(Dialog, Done);
end;

{ One list for both, because the two differ only in what a row says and what
  editing a row opens. IsCond picks between them. }
procedure TEditorApp.EditEventList(var E: TWorldEvent; IsCond: Boolean);
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Items: PStringCollection;
  Control: Word;
  I, Sel, Count, Limit: Integer;
  Title: string;
begin
  repeat
    if IsCond then
    begin
      Count := E.CondCount;
      Limit := MAX_CONDITIONS;
      Title := 'Conditions (all must hold)';
    end
    else
    begin
      Count := E.ActionCount;
      Limit := MAX_ACTIONS;
      Title := 'Actions (they run in order)';
    end;

    Items := New(PStringCollection, Init(10, 4));
    for I := 1 to Count do
      if IsCond then
        Items^.Insert(NewStr(Format('%d. %s %d, %d%s',
          [I, ConditionName(E.Conditions[I].CondType),
           E.Conditions[I].TargetID, E.Conditions[I].Value,
           BoolText(E.Conditions[I].Negate, '  (NOT)', '')])))
      else
        Items^.Insert(NewStr(Format('%d. %s %d, %d  %s',
          [I, ActionName(E.Actions[I].ActionType),
           E.Actions[I].TargetID, E.Actions[I].Value,
           E.Actions[I].Text])));
    if Count = 0 then
      Items^.Insert(NewStr('(none yet - press Add)'));

    R.Assign(8, 4, 72, 20);
    Dialog := New(PListDialog, Init(R, Title));
    with Dialog^ do
    begin
      R.Assign(60, 2, 61, 11);
      ScrollBar := New(PScrollBar, Init(R));
      R.Assign(2, 2, 60, 11);
      ListBox := New(PListBox, Init(R, 1, ScrollBar));
      ListBox^.NewList(Items);
      Insert(ListBox);
      Insert(ScrollBar);

      R.Assign(3, 12, 13, 14);
      Insert(New(PButton, Init(R, '~A~dd', cmEvItemAdd, bfNormal)));
      R.Assign(16, 12, 26, 14);
      Insert(New(PButton, Init(R, '~E~dit', cmEvItemEdit, bfDefault)));
      R.Assign(29, 12, 41, 14);
      Insert(New(PButton, Init(R, '~D~elete', cmEvItemDel, bfNormal)));
      R.Assign(48, 12, 60, 14);
      Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
    end;

    Control := Desktop^.ExecView(Dialog);
    Sel := ListBox^.Focused;
    Dispose(Dialog, Done);

    case Control of
      cmEvItemAdd:
        if Count >= Limit then
          MessageBox('All slots are used. Two events can share a trigger, ' +
                     'which is how you write more than this.', nil,
                     mfInformation + mfOKButton)
        else if IsCond then
        begin
          Inc(E.CondCount);
          E.Conditions[E.CondCount].CondType := ctNone;
          E.Conditions[E.CondCount].TargetID := 0;
          E.Conditions[E.CondCount].Value := 0;
          E.Conditions[E.CondCount].Negate := False;
          if not EditOneCondition(E.Conditions[E.CondCount]) then
            Dec(E.CondCount)
          else
            Modified := True;
        end
        else
        begin
          Inc(E.ActionCount);
          E.Actions[E.ActionCount].ActionType := atNone;
          E.Actions[E.ActionCount].TargetID := 0;
          E.Actions[E.ActionCount].Value := 0;
          E.Actions[E.ActionCount].Text := '';
          if not EditOneAction(E.Actions[E.ActionCount]) then
            Dec(E.ActionCount)
          else
            Modified := True;
        end;
      cmEvItemEdit:
        if (Sel >= 0) and (Sel < Count) then
        begin
          if IsCond then
          begin
            if EditOneCondition(E.Conditions[Sel + 1]) then Modified := True;
          end
          else
            if EditOneAction(E.Actions[Sel + 1]) then Modified := True;
        end;
      cmEvItemDel:
        if (Sel >= 0) and (Sel < Count) then
        begin
          { Closing the gap rather than blanking it: unlike an event slot,
            nothing outside the event refers to a condition or action by
            position. }
          if IsCond then
          begin
            for I := Sel + 1 to E.CondCount - 1 do
              E.Conditions[I] := E.Conditions[I + 1];
            Dec(E.CondCount);
          end
          else
          begin
            for I := Sel + 1 to E.ActionCount - 1 do
              E.Actions[I] := E.Actions[I + 1];
            Dec(E.ActionCount);
          end;
          Modified := True;
        end;
    end;
  until Control = cmCancel;
end;

procedure TEditorApp.EditEventBySlot(Slot: Integer);
var
  Dialog: PDialog;
  R: TRect;
  NameField, ID1Field, ID2Field: PInputLine;
  OptBox: PCheckBoxes;
  Items: PStringCollection;
  T: TEventTrigger;
  Control, Flags: Word;
  Idx: Integer;
  S: string;
  E: TWorldEvent;
  Finished: Boolean;
begin
  E := World.Events[Slot];

  repeat
    Finished := True;

    R.Assign(6, 3, 74, 22);
    Dialog := New(PListDialog, Init(R, 'Event ' + IntToStr(Slot)));
    ID1Field := nil;
    ID2Field := nil;

    with Dialog^ do
    begin
      R.Assign(2, 2, 12, 3);
      Insert(New(PStaticText, Init(R, 'Name:')));
      R.Assign(13, 2, 64, 3);
      NameField := New(PInputLine, Init(R, MAX_EVENT_NAME));
      S := E.Name;
      SetFieldStr(NameField, S);
      Insert(NameField);

      R.Assign(2, 4, 12, 5);
      Insert(New(PStaticText, Init(R, 'Trigger:')));
      R.Assign(13, 4, 64, 5);
      Insert(New(PStaticText, Init(R, TriggerName(E.TriggerType) + ' - ' +
                                     TriggerHelp(E.TriggerType))));

      R.Assign(2, 6, 23, 7);
      Insert(New(PStaticText, Init(R, TriggerLabel1(E.TriggerType))));
      R.Assign(24, 6, 34, 7);
      ID1Field := New(PInputLine, Init(R, 6));
      S := IntToStr(E.TriggerID);
      SetFieldStr(ID1Field, S);
      Insert(ID1Field);

      if TriggerLabel2(E.TriggerType) <> '' then
      begin
        R.Assign(2, 8, 23, 9);
        Insert(New(PStaticText, Init(R, TriggerLabel2(E.TriggerType))));
        R.Assign(24, 8, 34, 9);
        ID2Field := New(PInputLine, Init(R, 6));
        S := IntToStr(E.TriggerID2);
        SetFieldStr(ID2Field, S);
        Insert(ID2Field);
      end
      else
      begin
        R.Assign(2, 8, 64, 9);
        Insert(New(PStaticText, Init(R,
          'This trigger has no second ID.')));
      end;

      R.Assign(2, 10, 46, 12);
      OptBox := New(PCheckBoxes, Init(R,
        NewSItem('Fires ~o~nce only',
        NewSItem('~E~nabled when the game starts', nil))));
      Flags := 0;
      if E.OneShot then Flags := Flags or 1;
      if E.Enabled then Flags := Flags or 2;
      OptBox^.SetData(Flags);
      Insert(OptBox);

      R.Assign(2, 13, 64, 14);
      Insert(New(PStaticText, Init(R,
        'Slot ' + IntToStr(Slot) + ' is this event''s identity: save games ' +
        'record it.')));

      R.Assign(2, 15, 18, 17);
      Insert(New(PButton, Init(R, '~T~rigger...', cmEvTrigger, bfNormal)));
      R.Assign(20, 15, 36, 17);
      Insert(New(PButton, Init(R, 'C~o~nditions...', cmEvConds, bfNormal)));
      R.Assign(38, 15, 52, 17);
      Insert(New(PButton, Init(R, '~A~ctions...', cmEvActs, bfNormal)));

      R.Assign(2, 17, 12, 19);
      Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
      R.Assign(15, 17, 27, 19);
      Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
    end;

    Control := Desktop^.ExecView(Dialog);

    { Read the fields back before acting on any button, or a name typed
      just before pressing Conditions would be lost }
    if Control <> cmCancel then
    begin
      S := '';
      S := GetFieldStr(NameField);
      E.Name := S;
      S := '';
      S := GetFieldStr(ID1Field);
      E.TriggerID := StrToIntDef(S, 0);
      if ID2Field <> nil then
      begin
        S := '';
        S := GetFieldStr(ID2Field);
        E.TriggerID2 := StrToIntDef(S, 0);
      end
      else
        E.TriggerID2 := 0;
      OptBox^.GetData(Flags);
      E.OneShot := (Flags and 1) <> 0;
      E.Enabled := (Flags and 2) <> 0;
    end;

    Dispose(Dialog, Done);

    case Control of
      cmEvTrigger:
        begin
          Items := New(PStringCollection, Init(16, 4));
          for T := Low(TEventTrigger) to High(TEventTrigger) do
            Items^.Insert(NewStr(TriggerName(T) + ' - ' + TriggerHelp(T)));
          Idx := PickIndex('Trigger', Items, Ord(E.TriggerType));
          if Idx >= 0 then
          begin
            { The IDs mean something else now, and carrying the old numbers
              over would quietly point the event at the wrong thing }
            E.TriggerType := TEventTrigger(Idx);
            E.TriggerID := 0;
            E.TriggerID2 := 0;
          end;
          Finished := False;
        end;
      cmEvConds:
        begin
          EditEventList(E, True);
          Finished := False;
        end;
      cmEvActs:
        begin
          EditEventList(E, False);
          Finished := False;
        end;
      cmOK:
        begin
          E.Active := True;
          World.Events[Slot] := E;
          if World.EventCount < Slot then World.EventCount := Slot;
          SeedEventState(World);
          Modified := True;
        end;
    end;
  until Finished;
end;

procedure TEditorApp.AddEvent;
var
  Slot, I: Integer;
begin
  Slot := 0;
  for I := 1 to MAX_EVENTS do
    if not World.Events[I].Active then
    begin
      Slot := I;
      Break;
    end;

  if Slot = 0 then
  begin
    MessageBox('All event slots are used.', nil, mfError + mfOKButton);
    Exit;
  end;

  InitEvent(World.Events[Slot]);
  EditEventBySlot(Slot);
end;

procedure TEditorApp.ListEvents;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Control: Word;
  I, Count, Sel: Integer;
  Items: PStringCollection;
  Slots: array[1..MAX_EVENTS] of Integer;
begin
  Items := New(PStringCollection, Init(10, 10));
  Count := 0;

  for I := 1 to MAX_EVENTS do
    if World.Events[I].Active then
    begin
      Inc(Count);
      Slots[Count] := I;
      Items^.Insert(NewStr(Format('%3d: %-26s %-14s %s',
        [I, World.Events[I].Name, TriggerName(World.Events[I].TriggerType),
         BoolText(World.Events[I].Enabled, '', 'off')])));
    end;

  if Count = 0 then
  begin
    if MessageBox('No events yet. Add one now?', nil,
                  mfConfirmation + mfYesButton + mfNoButton) = cmYes then
      AddEvent;
    Dispose(Items, Done);
    Exit;
  end;

  R.Assign(4, 3, 76, 22);
  Dialog := New(PListDialog, Init(R, 'Events'));

  with Dialog^ do
  begin
    R.Assign(68, 2, 69, 15);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 68, 15);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(6, 15, 16, 17);
    Insert(New(PButton, Init(R, '~E~dit', cmEditEvent, bfDefault)));
    R.Assign(20, 15, 30, 17);
    Insert(New(PButton, Init(R, '~A~dd', cmAddEvent, bfNormal)));
    R.Assign(34, 15, 46, 17);
    Insert(New(PButton, Init(R, '~D~elete', cmDeleteEvent, bfNormal)));
    R.Assign(52, 15, 64, 17);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);
  Sel := ListBox^.Focused;
  Dispose(Dialog, Done);

  if (Control = cmEditEvent) and (Sel >= 0) and (Sel < Count) then
    EditEventBySlot(Slots[Sel + 1])
  else if Control = cmAddEvent then
    AddEvent
  else if (Control = cmDeleteEvent) and (Sel >= 0) and (Sel < Count) then
  begin
    if MessageBox('Delete this event? Slot ' + IntToStr(Slots[Sel + 1]) +
                  ' stays empty, so existing save games keep meaning what ' +
                  'they meant.', nil,
                  mfWarning + mfYesButton + mfNoButton) = cmYes then
    begin
      InitEvent(World.Events[Slots[Sel + 1]]);
      World.EventCount := 0;
      for I := MAX_EVENTS downto 1 do
        if World.Events[I].Active then
        begin
          World.EventCount := I;
          Break;
        end;
      SeedEventState(World);
      Modified := True;
    end;
  end;
end;

procedure TEditorApp.WorldSettings;
var
  Dialog: PDialog;
  R: TRect;
  TitleField, StartRoomField: PInputLine;
  WinRoomField, WinObjField: PInputLine;
  IntroField, WinParaField, LoseParaField: PInputLine;
  BookletCheck: PCheckBoxes;
  Control: Word;
  TitleStr, StartRoomStr: string;
  WinRoomStr, WinObjStr: string;
  IntroStr, WinParaStr, LoseParaStr: string;
  BookletVal: Word;
begin
  { Initialize string variables for SetData }
  TitleStr := World.Title;
  StartRoomStr := IntToStr(World.CurrentRoom);
  WinRoomStr := IntToStr(World.WinRoomID);
  WinObjStr := IntToStr(World.WinObjectID);
  IntroStr := IntToStr(World.IntroPara);
  WinParaStr := IntToStr(World.WinPara);
  LoseParaStr := IntToStr(World.LosePara);
  if (World.WorldFlags and WF_BOOKLET) <> 0 then
    BookletVal := $01
  else
    BookletVal := $00;

  { Create dialog }
  R.Assign(10, 2, 70, 24);
  Dialog := New(PDialog, Init(R, 'World Settings'));

  with Dialog^ do
  begin
    { Title }
    R.Assign(2, 2, 15, 3);
    Insert(New(PStaticText, Init(R, 'World Title:')));
    R.Assign(16, 2, 45, 3);
    TitleField := New(PInputLine, Init(R, MAX_NAME_LEN));
    SetFieldStr(TitleField, TitleStr);
    Insert(TitleField);

    { Start Room }
    R.Assign(2, 4, 15, 5);
    Insert(New(PStaticText, Init(R, 'Start Room ID:')));
    R.Assign(16, 4, 26, 5);
    StartRoomField := New(PInputLine, Init(R, 5));
    SetFieldStr(StartRoomField, StartRoomStr);
    Insert(StartRoomField);

    { Win condition }
    R.Assign(2, 6, 15, 7);
    Insert(New(PStaticText, Init(R, 'Win Room ID:')));
    R.Assign(16, 6, 26, 7);
    WinRoomField := New(PInputLine, Init(R, 5));
    SetFieldStr(WinRoomField, WinRoomStr);
    Insert(WinRoomField);

    R.Assign(2, 8, 15, 9);
    Insert(New(PStaticText, Init(R, 'Win Object ID:')));
    R.Assign(16, 8, 26, 9);
    WinObjField := New(PInputLine, Init(R, 5));
    SetFieldStr(WinObjField, WinObjStr);
    Insert(WinObjField);

    R.Assign(2, 10, 56, 11);
    Insert(New(PStaticText, Init(R,
      'Won by reaching Win Room carrying Win Object (0 = off).')));

    { Story paragraphs }
    R.Assign(2, 12, 15, 13);
    Insert(New(PStaticText, Init(R, 'Intro Para:')));
    R.Assign(16, 12, 26, 13);
    IntroField := New(PInputLine, Init(R, 5));
    SetFieldStr(IntroField, IntroStr);
    Insert(IntroField);

    R.Assign(30, 12, 43, 13);
    Insert(New(PStaticText, Init(R, 'Win Para:')));
    R.Assign(44, 12, 54, 13);
    WinParaField := New(PInputLine, Init(R, 5));
    SetFieldStr(WinParaField, WinParaStr);
    Insert(WinParaField);

    R.Assign(2, 14, 15, 15);
    Insert(New(PStaticText, Init(R, 'Lose Para:')));
    R.Assign(16, 14, 26, 15);
    LoseParaField := New(PInputLine, Init(R, 5));
    SetFieldStr(LoseParaField, LoseParaStr);
    Insert(LoseParaField);

    R.Assign(30, 14, 56, 15);
    Insert(New(PStaticText, Init(R, 'Shown on quitting unwon.')));

    R.Assign(2, 16, 56, 17);
    BookletCheck := New(PCheckBoxes, Init(R,
      NewSItem('Booklet mode: cite paragraph numbers, do not print text',
      nil)));
    BookletCheck^.SetData(BookletVal);
    Insert(BookletCheck);

    { Info }
    R.Assign(2, 18, 56, 19);
    Insert(New(PStaticText, Init(R,
      Format('Rooms: %d/%d  Objects: %d/%d  Mobs: %d/%d  Score: %d',
             [World.RoomCount, MAX_ROOMS,
              World.ObjectCount, MAX_OBJECTS,
              World.MobCount, MAX_MOBS,
              ComputeMaxScore(World)]))));

    { Buttons }
    R.Assign(12, 19, 22, 21);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(28, 19, 38, 21);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    TitleStr := '';
    StartRoomStr := '';
    WinRoomStr := '';
    WinObjStr := '';

    TitleStr := GetFieldStr(TitleField);
    StartRoomStr := GetFieldStr(StartRoomField);
    WinRoomStr := GetFieldStr(WinRoomField);
    WinObjStr := GetFieldStr(WinObjField);
    IntroStr := GetFieldStr(IntroField);
    WinParaStr := GetFieldStr(WinParaField);
    LoseParaStr := GetFieldStr(LoseParaField);
    BookletCheck^.GetData(BookletVal);

    { An empty title only rejects the title - it used to discard the start
      room and win condition along with it }
    if TitleStr <> '' then
      World.Title := TitleStr
    else
      MessageBox('World title cannot be empty - title left unchanged.', nil,
                 mfWarning + mfOKButton);

    World.CurrentRoom := StrToIntDef(StartRoomStr, 1);
    World.WinRoomID := StrToIntDef(WinRoomStr, 0);
    World.WinObjectID := StrToIntDef(WinObjStr, 0);
    World.IntroPara := StrToIntDef(IntroStr, 0);
    World.WinPara := StrToIntDef(WinParaStr, 0);
    World.LosePara := StrToIntDef(LoseParaStr, 0);
    if (BookletVal and $01) <> 0 then
      World.WorldFlags := World.WorldFlags or WF_BOOKLET
    else
      World.WorldFlags := World.WorldFlags and not WF_BOOKLET;
    Modified := True;

    MessageBox('World settings updated!', nil, mfInformation + mfOKButton);
  end;

  Dispose(Dialog, Done);
end;

{ Offers to pair up the exits of a room that was just added or edited. Shares
  PairExits with the lightweight editor, so both offer the same thing. }
procedure TEditorApp.OfferReverseExits(RoomIdx: Integer);
var
  Count: Integer;
begin
  Count := PairExits(World, RoomIdx, False);
  if Count = 0 then Exit;

  if MessageBox('Create ' + IntToStr(Count) +
                ' matching return exit(s)?', nil,
                mfConfirmation + mfYesButton + mfNoButton) = cmYes then
  begin
    PairExits(World, RoomIdx, True);
    Modified := True;
  end;
end;

{ The same checks the lightweight editor and the browser editor run, so a world
  that passes in one passes in all three. Read-only: it reports, it never
  edits, because the fix for most of these is an authoring decision. }
procedure TEditorApp.ValidateWorldDlg;
var
  Dialog: PDialog;
  R: TRect;
  ListBox: PListBox;
  ScrollBar: PScrollBar;
  Items: PStringCollection;
  Count, Errors, I: Integer;
begin
  Count := ValidateWorld(World, Issues);

  if Count = 0 then
  begin
    MessageBox('No problems found.', nil, mfInformation + mfOKButton);
    Exit;
  end;

  Errors := 0;
  Items := New(PStringCollection, Init(Count, 10));
  for I := 1 to Count do
  begin
    if Issues[I].Level = ilError then Inc(Errors);
    Items^.Insert(NewStr(IssueLevelName(Issues[I].Level) + ' [' +
                         Issues[I].Where + '] ' + Issues[I].Text));
  end;

  R.Assign(2, 2, 78, 22);
  Dialog := New(PDialog, Init(R, 'World Check - ' + IntToStr(Errors) +
                              ' error(s), ' + IntToStr(Count - Errors) +
                              ' warning(s)'));

  with Dialog^ do
  begin
    R.Assign(72, 2, 73, 16);
    ScrollBar := New(PScrollBar, Init(R));
    R.Assign(2, 2, 72, 16);
    ListBox := New(PListBox, Init(R, 1, ScrollBar));
    ListBox^.NewList(Items);
    Insert(ListBox);
    Insert(ScrollBar);

    R.Assign(32, 17, 42, 19);
    Insert(New(PButton, Init(R, '~C~lose', cmCancel, bfDefault)));
  end;

  Desktop^.ExecView(Dialog);
  Dispose(Dialog, Done);
end;

procedure TEditorApp.ShowAbout;
var
  AboutText: string;
begin
  AboutText := #13 +
               'Secret Orb World Editor' + #13 +
               'Turbo Vision Edition' + #13 +
               #13 +
               'Version ' + VERSION + #13 +
               #13 +
               'A professional world editor for' + #13 +
               'creating Secret Orb adventure games.' + #13 +
               #13 +
               'Supports Binary, Text, and BPL formats.' + #13 +
               #13 +
               'Based on Free Pascal Vision';

  MessageBox(AboutText, nil, mfInformation + mfOKButton);
end;

var
  EditorApp: TEditorApp;

begin
  EditorApp.Init;
  EditorApp.Run;
  EditorApp.Done;
end.
