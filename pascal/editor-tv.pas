{ Secret Orb World Editor - Turbo Vision Edition }
{ Professional TUI-based world editor using Free Pascal Vision }
program EditorTV;

{$MODE OBJFPC}{$H+}

uses
  Objects, Drivers, Views, Menus, App, Dialogs, MsgBox, Editors, SysUtils,
  GameData, DataFile;

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

  cmListParas     = 600;
  cmAddPara       = 601;
  cmEditPara      = 602;
  cmDeletePara    = 603;
  cmExportBooklet = 604;

type
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

    { World operations }
    procedure WorldSettings;
    procedure ShowAbout;
  end;

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
      nil))))),
    NewSubMenu('~W~orld', hcNoContext, NewMenu(
      NewItem('~S~ettings...', '', kbNoKey, cmWorldSettings, hcNoContext,
      NewLine(
      NewItem('~A~bout...', '', kbNoKey, cmAbout, hcNoContext,
      nil)))),
    nil)))))))));
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

      cmWorldSettings: WorldSettings;
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
    InputField^.GetData(Filename);

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
    InputField^.SetData(DefaultFile);
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
    InputField^.GetData(Filename);

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
    InputField^.SetData(DefaultFile);
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
    InputField^.GetData(Filename);

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
    InputField^.SetData(DefaultFile);
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
    InputField^.GetData(Filename);

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
  Dialog := New(PDialog, Init(R, 'Room List'));

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
    NorthField^.SetData(ZeroStr);
    Insert(NorthField);

    R.Assign(2, 9, 14, 10);
    Insert(New(PStaticText, Init(R, 'South Exit:')));
    R.Assign(15, 9, 25, 10);
    SouthField := New(PInputLine, Init(R, 5));
    SouthField^.SetData(ZeroStr);
    Insert(SouthField);

    R.Assign(2, 11, 14, 12);
    Insert(New(PStaticText, Init(R, 'East Exit:')));
    R.Assign(15, 11, 25, 12);
    EastField := New(PInputLine, Init(R, 5));
    EastField^.SetData(ZeroStr);
    Insert(EastField);

    R.Assign(2, 13, 14, 14);
    Insert(New(PStaticText, Init(R, 'West Exit:')));
    R.Assign(15, 13, 25, 14);
    WestField := New(PInputLine, Init(R, 5));
    WestField^.SetData(ZeroStr);
    Insert(WestField);

    R.Assign(2, 15, 14, 16);
    Insert(New(PStaticText, Init(R, 'Up Exit:')));
    R.Assign(15, 15, 25, 16);
    UpField := New(PInputLine, Init(R, 5));
    UpField^.SetData(ZeroStr);
    Insert(UpField);

    R.Assign(2, 17, 14, 18);
    Insert(New(PStaticText, Init(R, 'Down Exit:')));
    R.Assign(15, 17, 25, 18);
    DownField := New(PInputLine, Init(R, 5));
    DownField^.SetData(ZeroStr);
    Insert(DownField);

    { Score awarded on first visit }
    R.Assign(32, 7, 44, 8);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(45, 7, 55, 8);
    PointsField := New(PInputLine, Init(R, 5));
    PointsField^.SetData(ZeroStr);
    Insert(PointsField);

    R.Assign(32, 9, 66, 10);
    Insert(New(PStaticText, Init(R, 'Scored on first visit only.')));

    { Story paragraph played on first arrival }
    R.Assign(32, 11, 44, 12);
    Insert(New(PStaticText, Init(R, 'First Visit:')));
    R.Assign(45, 11, 55, 12);
    FirstVisitField := New(PInputLine, Init(R, 5));
    FirstVisitField^.SetData(ZeroStr);
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
    NameField^.GetData(RoomName);
    DescField^.GetData(RoomDesc);

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
    NorthField^.GetData(NorthStr);
    SouthField^.GetData(SouthStr);
    EastField^.GetData(EastStr);
    WestField^.GetData(WestStr);
    UpField^.GetData(UpStr);
    DownField^.GetData(DownStr);

    Room.Exits[dirNorth] := StrToIntDef(NorthStr, 0);
    Room.Exits[dirSouth] := StrToIntDef(SouthStr, 0);
    Room.Exits[dirEast] := StrToIntDef(EastStr, 0);
    Room.Exits[dirWest] := StrToIntDef(WestStr, 0);
    Room.Exits[dirUp] := StrToIntDef(UpStr, 0);
    Room.Exits[dirDown] := StrToIntDef(DownStr, 0);

    PointsStr := '';
    PointsField^.GetData(PointsStr);
    Room.Points := StrToIntDef(PointsStr, 0);

    FirstVisitStr := '';
    FirstVisitField^.GetData(FirstVisitStr);
    Room.FirstVisitPara := StrToIntDef(FirstVisitStr, 0);

    { Add room to world }
    Inc(World.RoomCount);
    World.Rooms[World.RoomCount] := Room;
    Modified := True;

    MessageBox('Room added successfully!', nil, mfInformation + mfOKButton);
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
    NameField^.SetData(RoomName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 14, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(15, 4, 65, 5);
    DescField := New(PInputLine, Init(R, MAX_DESC_LEN));
    DescField^.SetData(RoomDesc);
    Insert(DescField);

    { Exits }
    R.Assign(2, 7, 14, 8);
    Insert(New(PStaticText, Init(R, 'North Exit:')));
    R.Assign(15, 7, 25, 8);
    NorthField := New(PInputLine, Init(R, 5));
    NorthField^.SetData(NorthStr);
    Insert(NorthField);

    R.Assign(2, 9, 14, 10);
    Insert(New(PStaticText, Init(R, 'South Exit:')));
    R.Assign(15, 9, 25, 10);
    SouthField := New(PInputLine, Init(R, 5));
    SouthField^.SetData(SouthStr);
    Insert(SouthField);

    R.Assign(2, 11, 14, 12);
    Insert(New(PStaticText, Init(R, 'East Exit:')));
    R.Assign(15, 11, 25, 12);
    EastField := New(PInputLine, Init(R, 5));
    EastField^.SetData(EastStr);
    Insert(EastField);

    R.Assign(2, 13, 14, 14);
    Insert(New(PStaticText, Init(R, 'West Exit:')));
    R.Assign(15, 13, 25, 14);
    WestField := New(PInputLine, Init(R, 5));
    WestField^.SetData(WestStr);
    Insert(WestField);

    R.Assign(2, 15, 14, 16);
    Insert(New(PStaticText, Init(R, 'Up Exit:')));
    R.Assign(15, 15, 25, 16);
    UpField := New(PInputLine, Init(R, 5));
    UpField^.SetData(UpStr);
    Insert(UpField);

    R.Assign(2, 17, 14, 18);
    Insert(New(PStaticText, Init(R, 'Down Exit:')));
    R.Assign(15, 17, 25, 18);
    DownField := New(PInputLine, Init(R, 5));
    DownField^.SetData(DownStr);
    Insert(DownField);

    { Score awarded on first visit }
    R.Assign(32, 7, 44, 8);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(45, 7, 55, 8);
    PointsField := New(PInputLine, Init(R, 5));
    PointsField^.SetData(PointsStr);
    Insert(PointsField);

    R.Assign(32, 9, 66, 10);
    Insert(New(PStaticText, Init(R, 'Scored on first visit only.')));

    { Story paragraph played on first arrival }
    R.Assign(32, 11, 44, 12);
    Insert(New(PStaticText, Init(R, 'First Visit:')));
    R.Assign(45, 11, 55, 12);
    FirstVisitField := New(PInputLine, Init(R, 5));
    FirstVisitField^.SetData(FirstVisitStr);
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
    NameField^.GetData(RoomName);
    DescField^.GetData(RoomDesc);

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
    NorthField^.GetData(NorthStr);
    SouthField^.GetData(SouthStr);
    EastField^.GetData(EastStr);
    WestField^.GetData(WestStr);
    UpField^.GetData(UpStr);
    DownField^.GetData(DownStr);

    Room.Exits[dirNorth] := StrToIntDef(NorthStr, 0);
    Room.Exits[dirSouth] := StrToIntDef(SouthStr, 0);
    Room.Exits[dirEast] := StrToIntDef(EastStr, 0);
    Room.Exits[dirWest] := StrToIntDef(WestStr, 0);
    Room.Exits[dirUp] := StrToIntDef(UpStr, 0);
    Room.Exits[dirDown] := StrToIntDef(DownStr, 0);

    PointsStr := '';
    PointsField^.GetData(PointsStr);
    Room.Points := StrToIntDef(PointsStr, 0);

    FirstVisitStr := '';
    FirstVisitField^.GetData(FirstVisitStr);
    Room.FirstVisitPara := StrToIntDef(FirstVisitStr, 0);

    { Update room in world }
    World.Rooms[Index] := Room;
    Modified := True;

    MessageBox('Room updated successfully!', nil, mfInformation + mfOKButton);
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
  Dialog := New(PDialog, Init(R, 'Object List'));

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
    RoomIDField^.SetData(ZeroStr);
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
    PointsField^.SetData(ZeroStr);
    Insert(PointsField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Scored on first take only.')));

    { Story paragraph played on first take }
    R.Assign(26, 12, 36, 13);
    Insert(New(PStaticText, Init(R, 'First Take:')));
    R.Assign(37, 12, 47, 13);
    FirstTakeField := New(PInputLine, Init(R, 5));
    FirstTakeField^.SetData(ZeroStr);
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

    NameField^.GetData(ObjName);
    DescField^.GetData(ObjDesc);
    RoomIDField^.GetData(RoomIDStr);
    UseTextField^.GetData(UseTextStr);

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
    PointsField^.GetData(PointsStr);
    Obj.Points := StrToIntDef(PointsStr, 0);

    FirstTakeStr := '';
    FirstTakeField^.GetData(FirstTakeStr);
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
    NameField^.SetData(ObjName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    DescField^.SetData(ObjDesc);
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    RoomIDField^.SetData(RoomIDStr);
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
    UseTextField^.SetData(UseTextStr);
    Insert(UseTextField);

    { Score awarded on first take }
    R.Assign(26, 8, 36, 9);
    Insert(New(PStaticText, Init(R, 'Points:')));
    R.Assign(37, 8, 47, 9);
    PointsField := New(PInputLine, Init(R, 5));
    PointsField^.SetData(PointsStr);
    Insert(PointsField);

    R.Assign(26, 10, 56, 11);
    Insert(New(PStaticText, Init(R, 'Scored on first take only.')));

    { Story paragraph played on first take }
    R.Assign(26, 12, 36, 13);
    Insert(New(PStaticText, Init(R, 'First Take:')));
    R.Assign(37, 12, 47, 13);
    FirstTakeField := New(PInputLine, Init(R, 5));
    FirstTakeField^.SetData(FirstTakeStr);
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

    NameField^.GetData(ObjName);
    DescField^.GetData(ObjDesc);
    RoomIDField^.GetData(RoomIDStr);
    UseTextField^.GetData(UseTextStr);

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
    PointsField^.GetData(PointsStr);
    Obj.Points := StrToIntDef(PointsStr, 0);

    FirstTakeStr := '';
    FirstTakeField^.GetData(FirstTakeStr);
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
  Dialog := New(PDialog, Init(R, 'Mob List'));

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
    RoomIDField^.SetData(ZeroStr);
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
    FirstTalkField^.SetData(ZeroStr);
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

    NameField^.GetData(MobName);
    DescField^.GetData(MobDesc);
    RoomIDField^.GetData(RoomIDStr);
    DialogueField^.GetData(DialogueStr);
    FirstTalkField^.GetData(FirstTalkStr);

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
    NameField^.SetData(MobName);
    Insert(NameField);

    { Description }
    R.Assign(2, 4, 12, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(13, 4, 55, 5);
    DescField := New(PInputLine, Init(R, MAX_OBJ_DESC));
    DescField^.SetData(MobDesc);
    Insert(DescField);

    { Room ID }
    R.Assign(2, 6, 12, 7);
    Insert(New(PStaticText, Init(R, 'Room ID:')));
    R.Assign(13, 6, 23, 7);
    RoomIDField := New(PInputLine, Init(R, 5));
    RoomIDField^.SetData(RoomIDStr);
    Insert(RoomIDField);

    { Dialogue }
    R.Assign(2, 8, 12, 9);
    Insert(New(PStaticText, Init(R, 'Dialogue:')));
    R.Assign(13, 8, 55, 9);
    DialogueField := New(PInputLine, Init(R, MAX_DIALOGUE));
    DialogueField^.SetData(DialogueStr);
    Insert(DialogueField);

    { Story paragraph played the first time the player talks to this mob }
    R.Assign(2, 10, 13, 11);
    Insert(New(PStaticText, Init(R, 'First Talk:')));
    R.Assign(14, 10, 24, 11);
    FirstTalkField := New(PInputLine, Init(R, 5));
    FirstTalkField^.SetData(FirstTalkStr);
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

    NameField^.GetData(MobName);
    DescField^.GetData(MobDesc);
    RoomIDField^.GetData(RoomIDStr);
    DialogueField^.GetData(DialogueStr);
    FirstTalkField^.GetData(FirstTalkStr);

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
    NumField^.SetData(NumStr);
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
    NumField^.GetData(NumStr);
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
  Dialog := New(PDialog, Init(R, 'Story Paragraphs'));

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
    InputField^.SetData(DefaultFile);
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
    InputField^.GetData(Filename);

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
    TitleField^.SetData(TitleStr);
    Insert(TitleField);

    { Start Room }
    R.Assign(2, 4, 15, 5);
    Insert(New(PStaticText, Init(R, 'Start Room ID:')));
    R.Assign(16, 4, 26, 5);
    StartRoomField := New(PInputLine, Init(R, 5));
    StartRoomField^.SetData(StartRoomStr);
    Insert(StartRoomField);

    { Win condition }
    R.Assign(2, 6, 15, 7);
    Insert(New(PStaticText, Init(R, 'Win Room ID:')));
    R.Assign(16, 6, 26, 7);
    WinRoomField := New(PInputLine, Init(R, 5));
    WinRoomField^.SetData(WinRoomStr);
    Insert(WinRoomField);

    R.Assign(2, 8, 15, 9);
    Insert(New(PStaticText, Init(R, 'Win Object ID:')));
    R.Assign(16, 8, 26, 9);
    WinObjField := New(PInputLine, Init(R, 5));
    WinObjField^.SetData(WinObjStr);
    Insert(WinObjField);

    R.Assign(2, 10, 56, 11);
    Insert(New(PStaticText, Init(R,
      'Won by reaching Win Room carrying Win Object (0 = off).')));

    { Story paragraphs }
    R.Assign(2, 12, 15, 13);
    Insert(New(PStaticText, Init(R, 'Intro Para:')));
    R.Assign(16, 12, 26, 13);
    IntroField := New(PInputLine, Init(R, 5));
    IntroField^.SetData(IntroStr);
    Insert(IntroField);

    R.Assign(30, 12, 43, 13);
    Insert(New(PStaticText, Init(R, 'Win Para:')));
    R.Assign(44, 12, 54, 13);
    WinParaField := New(PInputLine, Init(R, 5));
    WinParaField^.SetData(WinParaStr);
    Insert(WinParaField);

    R.Assign(2, 14, 15, 15);
    Insert(New(PStaticText, Init(R, 'Lose Para:')));
    R.Assign(16, 14, 26, 15);
    LoseParaField := New(PInputLine, Init(R, 5));
    LoseParaField^.SetData(LoseParaStr);
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

    TitleField^.GetData(TitleStr);
    StartRoomField^.GetData(StartRoomStr);
    WinRoomField^.GetData(WinRoomStr);
    WinObjField^.GetData(WinObjStr);
    IntroField^.GetData(IntroStr);
    WinParaField^.GetData(WinParaStr);
    LoseParaField^.GetData(LoseParaStr);
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
