{ Secret Orb World Editor - Turbo Vision Edition }
{ Professional TUI-based world editor using Free Pascal Vision }
program EditorTV;

{$MODE OBJFPC}{$H+}

uses
  Objects, Drivers, Views, Menus, App, Dialogs, MsgBox, SysUtils,
  GameData, DataFile;

const
  VERSION = '0.2.0-TV';

  { Command constants }
  cmNewWorld     = 100;
  cmLoadWorld    = 101;
  cmSaveWorld    = 102;
  cmSaveWorldAs  = 103;

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
      NewItem('E~x~it', 'Alt+X', kbAltX, cmQuit, hcNoContext,
      nil))))))),
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
    NewSubMenu('~W~orld', hcNoContext, NewMenu(
      NewItem('~S~ettings...', '', kbNoKey, cmWorldSettings, hcNoContext,
      NewLine(
      NewItem('~A~bout...', '', kbNoKey, cmAbout, hcNoContext,
      nil)))),
    nil))))))));
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

      cmListRooms:     ListRooms;
      cmAddRoom:       AddRoom;

      cmListObjects:   ListObjects;
      cmAddObject:     AddObject;

      cmListMobs:      ListMobs;
      cmAddMob:        AddMob;

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
  Control: Word;
  Room: TRoom;
  RoomName, RoomDesc: string;
  NorthStr, SouthStr, EastStr, WestStr, UpStr, DownStr: string;
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
  Control: Word;
  Room: TRoom;
  RoomName, RoomDesc: string;
  NorthStr, SouthStr, EastStr, WestStr, UpStr, DownStr: string;
begin
  if (Index < 1) or (Index > MAX_ROOMS) or (not World.Rooms[Index].Active) then
    Exit;

  Room := World.Rooms[Index];

  { Initialize string variables for SetData }
  RoomName := Room.Name;
  RoomDesc := Room.Desc;
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
  PickupCheck: PCheckBoxes;
  Control: Word;
  Obj: TGameObject;
  ObjName, ObjDesc, RoomIDStr, UseTextStr: string;
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
  PickupCheck: PCheckBoxes;
  Control: Word;
  Obj: TGameObject;
  ObjName, ObjDesc, RoomIDStr, UseTextStr: string;
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
  Control: Word;
  Mob: TMob;
  MobName, MobDesc, RoomIDStr, DialogueStr: string;
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
  R.Assign(10, 5, 70, 17);
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

    { Buttons }
    R.Assign(15, 10, 25, 12);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 10, 40, 12);
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

    NameField^.GetData(MobName);
    DescField^.GetData(MobDesc);
    RoomIDField^.GetData(RoomIDStr);
    DialogueField^.GetData(DialogueStr);

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
  Control: Word;
  Mob: TMob;
  MobName, MobDesc, RoomIDStr, DialogueStr: string;
begin
  if (Index < 1) or (Index > MAX_MOBS) or (not World.Mobs[Index].Active) then
    Exit;

  Mob := World.Mobs[Index];

  { Initialize string variables for SetData }
  MobName := Mob.Name;
  MobDesc := Mob.Desc;
  RoomIDStr := IntToStr(Mob.RoomID);
  DialogueStr := Mob.Dialogue;

  { Create dialog }
  R.Assign(10, 5, 70, 17);
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

    { Buttons }
    R.Assign(15, 10, 25, 12);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 10, 40, 12);
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

    NameField^.GetData(MobName);
    DescField^.GetData(MobDesc);
    RoomIDField^.GetData(RoomIDStr);
    DialogueField^.GetData(DialogueStr);

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

procedure TEditorApp.WorldSettings;
var
  Dialog: PDialog;
  R: TRect;
  TitleField, StartRoomField: PInputLine;
  Control: Word;
  TitleStr, StartRoomStr: string;
begin
  { Initialize string variables for SetData }
  TitleStr := World.Title;
  StartRoomStr := IntToStr(World.CurrentRoom);

  { Create dialog }
  R.Assign(15, 8, 65, 17);
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

    { Info }
    R.Assign(2, 6, 45, 7);
    Insert(New(PStaticText, Init(R,
      Format('Rooms: %d/%d  Objects: %d/%d  Mobs: %d/%d',
             [World.RoomCount, MAX_ROOMS,
              World.ObjectCount, MAX_OBJECTS,
              World.MobCount, MAX_MOBS]))));

    { Buttons }
    R.Assign(10, 7, 20, 9);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(25, 7, 35, 9);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  Control := Desktop^.ExecView(Dialog);

  if Control = cmOK then
  begin
    { Get data }
    TitleStr := '';
    StartRoomStr := '';

    TitleField^.GetData(TitleStr);
    StartRoomField^.GetData(StartRoomStr);

    if TitleStr <> '' then
    begin
      World.Title := TitleStr;
      World.CurrentRoom := StrToIntDef(StartRoomStr, 1);
      Modified := True;

      MessageBox('World settings updated!', nil, mfInformation + mfOKButton);
    end;
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
