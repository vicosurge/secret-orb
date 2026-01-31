program SimpleTUI;

{$mode objfpc}{$H+}

uses
  Objects, Drivers, Views, Menus, App, Dialogs, MsgBox, SysUtils;

type
  TSimpleApp = object(TApplication)
    procedure InitStatusLine; virtual;
    procedure InitMenuBar; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { Binary file structures }
  TRoomFileHeader = packed record
    MagicNumber: array[0..3] of Char;  { 'ROOM' }
    Version: Word;                     { File format version }
    RecordCount: LongInt;              { Number of rooms }
    RecordSize: Word;                  { Size of each record }
    Reserved: LongInt;                 { For future use }
  end;

  TRoomRecord = packed record
    RoomID: LongInt;                   { Auto-assigned room ID }
    RoomName: array[0..50] of Char;    { 51 bytes: 50 chars + null }
    Description: array[0..200] of Char; { 201 bytes: 200 chars + null }
    NorthExit: LongInt;                { Room ID for north exit }
    SouthExit: LongInt;                { Room ID for south exit }
    EastExit: LongInt;                 { Room ID for east exit }
    WestExit: LongInt;                 { Room ID for west exit }
    UpExit: LongInt;                   { Room ID for up exit }
    DownExit: LongInt;                 { Room ID for down exit }
  end;

const
  cmFileOps = 101;
  cmFormRoom = 102;
  cmListRooms = 103;
  cmEditRooms = 104;
  ROOM_FILE_NAME = 'rooms.dat';
  MAGIC_NUMBER = 'ROOM';
  FILE_VERSION = 1;

{ Utility functions }
function Min(a, b: Integer): Integer;
begin
  if a < b then
    Min := a
  else
    Min := b;
end;

function FileExists(const FileName: string): Boolean;
var
  F: File;
begin
  Assign(F, FileName);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult = 0 then
  begin
    Close(F);
    FileExists := True;
  end
  else
    FileExists := False;
end;

procedure InitializeRoomFile;
var
  F: File;
  Header: TRoomFileHeader;
begin
  if not FileExists(ROOM_FILE_NAME) then
  begin
    Assign(F, ROOM_FILE_NAME);
    {$I-}
    Rewrite(F, 1);
    {$I+}
    if IOResult = 0 then
    begin
      { Initialize header }
      Header.MagicNumber := MAGIC_NUMBER;
      Header.Version := FILE_VERSION;
      Header.RecordCount := 0;
      Header.RecordSize := SizeOf(TRoomRecord);
      Header.Reserved := 0;
      
      BlockWrite(F, Header, SizeOf(Header));
      Close(F);
    end;
  end;
end;

function GetNextRoomID: LongInt;
var
  F: File;
  Header: TRoomFileHeader;
begin
  GetNextRoomID := 1;  { Default if file doesn't exist }
  
  if FileExists(ROOM_FILE_NAME) then
  begin
    Assign(F, ROOM_FILE_NAME);
    {$I-}
    Reset(F, 1);
    {$I+}
    if IOResult = 0 then
    begin
      BlockRead(F, Header, SizeOf(Header));
      GetNextRoomID := Header.RecordCount + 1;
      Close(F);
    end;
  end;
end;

procedure SaveRoomToFile(const Room: TRoomRecord);
var
  F: File;
  Header: TRoomFileHeader;
begin
  InitializeRoomFile;
  
  Assign(F, ROOM_FILE_NAME);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageBox('Error opening room file for writing!', nil, mfError + mfOKButton);
    Exit;
  end;
  
  { Read current header }
  BlockRead(F, Header, SizeOf(Header));
  
  { Verify file format }
  if Header.MagicNumber <> MAGIC_NUMBER then
  begin
    Close(F);
    MessageBox('Invalid room file format!', nil, mfError + mfOKButton);
    Exit;
  end;
  
  { Go to end of file to append new room }
  Seek(F, FileSize(F));
  
  { Write the room record }
  BlockWrite(F, Room, SizeOf(Room));
  
  { Update header with new record count }
  Inc(Header.RecordCount);
  Seek(F, 0);
  BlockWrite(F, Header, SizeOf(Header));
  
  Close(F);
  
  MessageBox('Room saved successfully! Room ID: ' + IntToStr(Room.RoomID), 
             nil, mfInformation + mfOKButton);
end;

procedure UpdateRoomInFile(const Room: TRoomRecord; RoomIndex: Integer);
var
  F: File;
  Header: TRoomFileHeader;
begin
  Assign(F, ROOM_FILE_NAME);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageBox('Error opening room file for editing!', nil, mfError + mfOKButton);
    Exit;
  end;
  
  { Read header to verify file }
  BlockRead(F, Header, SizeOf(Header));
  
  if Header.MagicNumber <> MAGIC_NUMBER then
  begin
    Close(F);
    MessageBox('Invalid room file format!', nil, mfError + mfOKButton);
    Exit;
  end;
  
  { Calculate position and update the room }
  Seek(F, SizeOf(Header) + ((RoomIndex - 1) * SizeOf(TRoomRecord)));
  BlockWrite(F, Room, SizeOf(Room));
  
  Close(F);
  
  MessageBox('Room updated successfully!', nil, mfInformation + mfOKButton);
end;

procedure FileOperations;
var
  filename: string;
  F: Text;
begin
  filename := 'test_output.txt';
  try
    Assign(F, filename);
    Rewrite(F);
    writeln(F, 'Hello from Free Vision TUI!');
    writeln(F, 'Generated at: ' + DateTimeToStr(Now));
    Close(F);
    MessageBox('File "' + filename + '" created successfully!', nil, mfInformation + mfOKButton);
  except
    on E: Exception do
      MessageBox('Error creating file: ' + E.Message, nil, mfError + mfOKButton);
  end;
end;

procedure StringToPChar(const Source: string; var Dest: array of Char);
var
  i: Integer;
begin
  FillChar(Dest, SizeOf(Dest), 0);  { Clear the array }
  for i := 1 to Min(Length(Source), High(Dest)) do
    Dest[i-1] := Source[i];
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then
    Result := Default;
end;

procedure CreateRoomFile;
var
  Dialog: PDialog;
  R: TRect;
  RoomNameField, DescriptionField, NorthField, SouthField, EastField, WestField, UpField, DownField: PInputLine;
  roomName, descriptionName, northID, southID, eastID, westID, upID, downID: string;
  result: Word;
  RoomRecord: TRoomRecord;
begin
  roomName := '';
  descriptionName := '';

  R.Assign(5, 4, 75, 30);
  Dialog := New(PDialog, Init(R, 'Room Information'));

  with Dialog^ do
  begin
    { Room Name and field }
    R.Assign(3, 2, 15, 3);
    Insert(New(PStaticText, Init(R, 'Room Name:')));
    R.Assign(16, 2, 55, 3);
    RoomNameField := New(PInputLine, Init(R, 50));
    Insert(RoomNameField);

    { Description and field }
    R.Assign(3, 4, 15, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(16, 4, 55, 5);
    DescriptionField := New(PInputLine, Init(R, 200));
    Insert(DescriptionField);

    { North and Field }
    R.Assign(3, 6, 15, 7);
    Insert(New(PStaticText, Init(R, 'North:')));
    R.Assign(16, 6, 25, 7);
    NorthField := New(PInputLine, Init(R, 10));
    Insert(NorthField);

    { South and Field }
    R.Assign(3, 8, 15, 9);
    Insert(New(PStaticText, Init(R, 'South:')));
    R.Assign(16, 8, 25, 9);
    SouthField := New(PInputLine, Init(R, 10));
    Insert(SouthField);

    { East and Field }
    R.Assign(3, 10, 15, 11);
    Insert(New(PStaticText, Init(R, 'East:')));
    R.Assign(16, 10, 25, 11);
    EastField := New(PInputLine, Init(R, 10));
    Insert(EastField);

    { West and Field }
    R.Assign(3, 12, 15, 13);
    Insert(New(PStaticText, Init(R, 'West:')));
    R.Assign(16, 12, 25, 13);
    WestField := New(PInputLine, Init(R, 10));
    Insert(WestField);

    { Up and Field }
    R.Assign(3, 14, 15, 15);
    Insert(New(PStaticText, Init(R, 'Up:')));
    R.Assign(16, 14, 25, 15);
    UpField := New(PInputLine, Init(R, 10));
    Insert(UpField);

    { Down and Field }
    R.Assign(3, 16, 15, 17);
    Insert(New(PStaticText, Init(R, 'Down:')));
    R.Assign(16, 16, 25, 17);
    DownField := New(PInputLine, Init(R, 10));
    Insert(DownField);

    { Buttons }
    R.Assign(15, 19, 25, 21);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 19, 40, 21);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  result := Desktop^.ExecView(Dialog);

  if result = cmOK then
  begin
    { Get data from fields }
    if RoomNameField^.Data <> nil then
      roomName := PShortString(RoomNameField^.Data)^;
    if DescriptionField^.Data <> nil then
      descriptionName := PShortString(DescriptionField^.Data)^;
    if NorthField^.Data <> nil then
      northID := PShortString(NorthField^.Data)^;
    if SouthField^.Data <> nil then
      southID := PShortString(SouthField^.Data)^;
    if EastField^.Data <> nil then
      eastID := PShortString(EastField^.Data)^;
    if WestField^.Data <> nil then
      westID := PShortString(WestField^.Data)^;
    if UpField^.Data <> nil then
      upID := PShortString(UpField^.Data)^;
    if DownField^.Data <> nil then
      downID := PShortString(DownField^.Data)^;

    { Create room record }
    FillChar(RoomRecord, SizeOf(RoomRecord), 0);
    RoomRecord.RoomID := GetNextRoomID;
    StringToPChar(roomName, RoomRecord.RoomName);
    StringToPChar(descriptionName, RoomRecord.Description);
    RoomRecord.NorthExit := StrToIntDef(northID, 0);
    RoomRecord.SouthExit := StrToIntDef(southID, 0);
    RoomRecord.EastExit := StrToIntDef(eastID, 0);
    RoomRecord.WestExit := StrToIntDef(westID, 0);
    RoomRecord.UpExit := StrToIntDef(upID, 0);
    RoomRecord.DownExit := StrToIntDef(downID, 0);

    { Save to binary file }
    SaveRoomToFile(RoomRecord);
  end;

  Dispose(Dialog, Done);
end;

procedure EditRoomForm(var Room: TRoomRecord);
var
  Dialog: PDialog;
  R: TRect;
  RoomNameField, DescriptionField, NorthField, SouthField, EastField, WestField, UpField, DownField: PInputLine;
  roomName, descriptionName, northID, southID, eastID, westID, upID, downID: string;
  result: Word;
  OriginalID: LongInt;
begin
  { Store original ID }
  OriginalID := Room.RoomID;
  
  { Get current values from the room record }
  Room.RoomName[50] := #0;
  Room.Description[200] := #0;
  roomName := StrPas(Room.RoomName);
  descriptionName := StrPas(Room.Description);
  
  { Convert exit IDs to strings, show empty for 0 }
  if Room.NorthExit = 0 then northID := '' else northID := IntToStr(Room.NorthExit);
  if Room.SouthExit = 0 then southID := '' else southID := IntToStr(Room.SouthExit);
  if Room.EastExit = 0 then eastID := '' else eastID := IntToStr(Room.EastExit);
  if Room.WestExit = 0 then westID := '' else westID := IntToStr(Room.WestExit);
  if Room.UpExit = 0 then upID := '' else upID := IntToStr(Room.UpExit);
  if Room.DownExit = 0 then downID := '' else downID := IntToStr(Room.DownExit);

  R.Assign(5, 4, 75, 30);
  Dialog := New(PDialog, Init(R, 'Edit Room ID: ' + IntToStr(Room.RoomID)));

  with Dialog^ do
  begin
    { Room Name and field }
    R.Assign(3, 2, 15, 3);
    Insert(New(PStaticText, Init(R, 'Room Name:')));
    R.Assign(16, 2, 55, 3);
    RoomNameField := New(PInputLine, Init(R, 50));
    RoomNameField^.SetData(roomName);
    Insert(RoomNameField);

    { Description and field }
    R.Assign(3, 4, 15, 5);
    Insert(New(PStaticText, Init(R, 'Description:')));
    R.Assign(16, 4, 55, 5);
    DescriptionField := New(PInputLine, Init(R, 200));
    DescriptionField^.SetData(descriptionName);
    Insert(DescriptionField);

    { North and Field }
    R.Assign(3, 6, 15, 7);
    Insert(New(PStaticText, Init(R, 'North:')));
    R.Assign(16, 6, 25, 7);
    NorthField := New(PInputLine, Init(R, 10));
    NorthField^.SetData(northID);
    Insert(NorthField);

    { South and Field }
    R.Assign(3, 8, 15, 9);
    Insert(New(PStaticText, Init(R, 'South:')));
    R.Assign(16, 8, 25, 9);
    SouthField := New(PInputLine, Init(R, 10));
    SouthField^.SetData(southID);
    Insert(SouthField);

    { East and Field }
    R.Assign(3, 10, 15, 11);
    Insert(New(PStaticText, Init(R, 'East:')));
    R.Assign(16, 10, 25, 11);
    EastField := New(PInputLine, Init(R, 10));
    EastField^.SetData(eastID);
    Insert(EastField);

    { West and Field }
    R.Assign(3, 12, 15, 13);
    Insert(New(PStaticText, Init(R, 'West:')));
    R.Assign(16, 12, 25, 13);
    WestField := New(PInputLine, Init(R, 10));
    WestField^.SetData(westID);
    Insert(WestField);

    { Up and Field }
    R.Assign(3, 14, 15, 15);
    Insert(New(PStaticText, Init(R, 'Up:')));
    R.Assign(16, 14, 25, 15);
    UpField := New(PInputLine, Init(R, 10));
    UpField^.SetData(upID);
    Insert(UpField);

    { Down and Field }
    R.Assign(3, 16, 15, 17);
    Insert(New(PStaticText, Init(R, 'Down:')));
    R.Assign(16, 16, 25, 17);
    DownField := New(PInputLine, Init(R, 10));
    DownField^.SetData(downID);
    Insert(DownField);

    { Buttons }
    R.Assign(15, 19, 25, 21);
    Insert(New(PButton, Init(R, '~S~ave', cmOK, bfDefault)));

    R.Assign(30, 19, 40, 21);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  result := Desktop^.ExecView(Dialog);

  if result = cmOK then
  begin
    { Get data from fields }
    RoomNameField^.GetData(roomName);
    DescriptionField^.GetData(descriptionName);
    NorthField^.GetData(northID);
    SouthField^.GetData(southID);
    EastField^.GetData(eastID);
    WestField^.GetData(westID);
    UpField^.GetData(upID);
    DownField^.GetData(downID);

    { Update room record }
    FillChar(Room, SizeOf(Room), 0);
    Room.RoomID := OriginalID; { Keep original ID }
    StringToPChar(roomName, Room.RoomName);
    StringToPChar(descriptionName, Room.Description);
    Room.NorthExit := StrToIntDef(northID, 0);
    Room.SouthExit := StrToIntDef(southID, 0);
    Room.EastExit := StrToIntDef(eastID, 0);
    Room.WestExit := StrToIntDef(westID, 0);
    Room.UpExit := StrToIntDef(upID, 0);
    Room.DownExit := StrToIntDef(downID, 0);
  end;

  Dispose(Dialog, Done);
end;

procedure ShowRoomDetails(const Room: TRoomRecord);
var
  Dialog: PDialog;
  R: TRect;
  DetailText: PStaticText;
  DetailString: string;
begin
  { Create a larger dialog for room details }
  R.Assign(5, 3, 75, 22);
  Dialog := New(PDialog, Init(R, 'Room Details'));

  with Dialog^ do
  begin
    { Create detailed room information }
    DetailString := 'Room ID: ' + IntToStr(Room.RoomID) + #13 +
                   'Name: ' + StrPas(Room.RoomName) + #13 +
                   'Description: ' + StrPas(Room.Description) + #13 +
                   '' + #13 +
                   'Exits:' + #13 +
                   '  North: ' + IntToStr(Room.NorthExit) + #13 +
                   '  South: ' + IntToStr(Room.SouthExit) + #13 +
                   '  East:  ' + IntToStr(Room.EastExit) + #13 +
                   '  West:  ' + IntToStr(Room.WestExit) + #13 +
                   '  Up:    ' + IntToStr(Room.UpExit) + #13 +
                   '  Down:  ' + IntToStr(Room.DownExit);

    { Multi-line text display }
    R.Assign(2, 2, 66, 16);
    DetailText := New(PStaticText, Init(R, DetailString));
    Insert(DetailText);

    { Close button }
    R.Assign(30, 17, 40, 19);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
  end;

  Desktop^.ExecView(Dialog);
  Dispose(Dialog, Done);
end;

procedure EditRooms;
var
  F: File;
  Header: TRoomFileHeader;
  Room: TRoomRecord;
  Dialog: PDialog;
  R: TRect;
  i: Integer;
  SelectedRoom: Integer;
  CleanName: string;
  RoomData: array[1..50] of TRoomRecord;
  RoomCount: Integer;
  ListText: PStaticText;
  DisplayText: string;
  InputField: PInputLine;
  RoomNumber: string;
  SelectedIndex: Integer;
begin
  if not FileExists(ROOM_FILE_NAME) then
  begin
    MessageBox('No room file found!', nil, mfError + mfOKButton);
    Exit;
  end;

  Assign(F, ROOM_FILE_NAME);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageBox('Error opening room file!', nil, mfError + mfOKButton);
    Exit;
  end;

  { Read header }
  BlockRead(F, Header, SizeOf(Header));
  
  if Header.RecordCount = 0 then
  begin
    Close(F);
    MessageBox('No rooms found in file!', nil, mfInformation + mfOKButton);
    Exit;
  end;

  RoomCount := Header.RecordCount;
  if RoomCount > 50 then RoomCount := 50;

  { Read all rooms into array }
  DisplayText := 'Select Room to Edit:' + #13#13;
  for i := 1 to RoomCount do
  begin
    BlockRead(F, Room, SizeOf(Room));
    RoomData[i] := Room;
    
    Room.RoomName[50] := #0;
    CleanName := StrPas(Room.RoomName);
    
    DisplayText := DisplayText + IntToStr(i) + '. Room ' + IntToStr(Room.RoomID) + 
                  ': ' + CleanName + #13;
  end;

  Close(F);

  { Create room selection dialog }
  R.Assign(10, 5, 70, 20);
  Dialog := New(PDialog, Init(R, 'Edit Room'));

  with Dialog^ do
  begin
    { Display room list }
    R.Assign(2, 2, 56, 12);
    ListText := New(PStaticText, Init(R, DisplayText));
    Insert(ListText);

    { Input field for room number }
    R.Assign(2, 13, 35, 14);
    Insert(New(PStaticText, Init(R, 'Enter room number to edit:')));
    
    R.Assign(36, 13, 46, 14);
    InputField := New(PInputLine, Init(R, 5));
    Insert(InputField);

    { Buttons }
    R.Assign(15, 15, 25, 17);
    Insert(New(PButton, Init(R, '~E~dit', cmOK, bfDefault)));

    R.Assign(30, 15, 40, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  SelectedRoom := Desktop^.ExecView(Dialog);

  if SelectedRoom = cmOK then
  begin
    { Get the room number from input }
    RoomNumber := '';
    if InputField^.Data <> nil then
      RoomNumber := PShortString(InputField^.Data)^;
    
    SelectedIndex := StrToIntDef(RoomNumber, 0);
    
    { Validate room number }
    if (SelectedIndex < 1) or (SelectedIndex > RoomCount) then
    begin
      MessageBox('Invalid room number! Please enter a number between 1 and ' + 
                 IntToStr(RoomCount), nil, mfError + mfOKButton);
    end
    else
    begin
      { Get the selected room }
      Room := RoomData[SelectedIndex];
      
      { Ensure null termination }
      Room.RoomName[50] := #0;
      Room.Description[200] := #0;
      
      { Edit the room }
      EditRoomForm(Room);
      
      { Save changes back to file }
      UpdateRoomInFile(Room, SelectedIndex);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure ListRooms;
var
  F: File;
  Header: TRoomFileHeader;
  Room: TRoomRecord;
  Dialog: PDialog;
  R: TRect;
  i: Integer;
  SelectedRoom: Integer;
  CleanName: string;
  RoomData: array[1..50] of TRoomRecord;
  RoomCount: Integer;
  ListText: PStaticText;
  DisplayText: string;
  InputField: PInputLine;
  RoomNumber: string;
  SelectedIndex: Integer;
begin
  if not FileExists(ROOM_FILE_NAME) then
  begin
    MessageBox('No room file found!', nil, mfError + mfOKButton);
    Exit;
  end;

  Assign(F, ROOM_FILE_NAME);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageBox('Error opening room file!', nil, mfError + mfOKButton);
    Exit;
  end;

  { Read header }
  BlockRead(F, Header, SizeOf(Header));
  
  if Header.RecordCount = 0 then
  begin
    Close(F);
    MessageBox('No rooms found in file!', nil, mfInformation + mfOKButton);
    Exit;
  end;

  RoomCount := Header.RecordCount;
  if RoomCount > 50 then RoomCount := 50;

  { Read all rooms into array }
  DisplayText := 'Rooms:' + #13#13;
  for i := 1 to RoomCount do
  begin
    BlockRead(F, Room, SizeOf(Room));
    RoomData[i] := Room;
    
    { Force null termination }
    Room.RoomName[50] := #0;
    CleanName := StrPas(Room.RoomName);
    
    { Build display text manually }
    DisplayText := DisplayText + IntToStr(i) + '. Room ' + IntToStr(Room.RoomID) + 
                  ': ' + CleanName + #13;
  end;

  Close(F);

  DisplayText := DisplayText + #13 + 'Use number keys 1-' + IntToStr(RoomCount) + ' to select a room';

  { Create dialog with static text instead of listbox }
  R.Assign(10, 5, 70, 20);
  Dialog := New(PDialog, Init(R, 'Room List'));

  with Dialog^ do
  begin
    { Static text display instead of listbox }
    R.Assign(2, 2, 56, 12);
    ListText := New(PStaticText, Init(R, DisplayText));
    Insert(ListText);

    { Input field for room number }
    R.Assign(2, 13, 30, 14);
    Insert(New(PStaticText, Init(R, 'Enter room number:')));
    
    R.Assign(31, 13, 41, 14);
    InputField := New(PInputLine, Init(R, 5));
    Insert(InputField);

    { Buttons }
    R.Assign(15, 15, 25, 17);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));

    R.Assign(30, 15, 40, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  SelectedRoom := Desktop^.ExecView(Dialog);

  { If user selected OK, get the room number }
  if SelectedRoom = cmOK then
  begin
    { Get the room number from input }
    RoomNumber := '';
    if InputField^.Data <> nil then
      RoomNumber := PShortString(InputField^.Data)^;
    
    SelectedIndex := StrToIntDef(RoomNumber, 0);
    
    { Validate room number }
    if (SelectedIndex < 1) or (SelectedIndex > RoomCount) then
    begin
      MessageBox('Invalid room number! Please enter a number between 1 and ' + 
                 IntToStr(RoomCount), nil, mfError + mfOKButton);
    end
    else
    begin
      { Get the selected room }
      Room := RoomData[SelectedIndex];
      
      { Ensure null termination before display }
      Room.RoomName[50] := #0;
      Room.Description[200] := #0;
      
      { Call the details procedure }
      ShowRoomDetails(Room);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure TSimpleApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt+X~ Exit', kbAltX, cmQuit, nil), nil)));
end;

procedure TSimpleApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~A~ctions', hcNoContext, NewMenu(
      NewItem('~F~ile Operations', '', kbNoKey, cmFileOps, hcNoContext,
      NewItem('~R~oom Form', '', kbNoKey, cmFormRoom, hcNoContext,
      NewItem('~L~ist Rooms', '', kbNoKey, cmListRooms, hcNoContext,
      NewItem('~E~dit Rooms', '', kbNoKey, cmEditRooms, hcNoContext,
      NewLine(
      NewItem('~Q~uit', 'Alt+X', kbAltX, cmQuit, hcNoContext,
      nil))))))),
    nil))));
end;

procedure TSimpleApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if Event.What = evCommand then
  begin
    case Event.Command of
      cmFileOps:   FileOperations;
      cmFormRoom:  CreateRoomFile;
      cmListRooms: ListRooms;
      cmEditRooms: EditRooms;
    else
      Exit;
    end;
    ClearEvent(Event);
  end;
end;

var
  MyApplication: TSimpleApp;

begin
  MyApplication.Init;
  MyApplication.Run;
  MyApplication.Done;
end.
  DisplayText := 'Select Room to Edit:' + #13#13;
  for i := 1 to RoomCount do
  begin
    BlockRead(F, Room, SizeOf(Room));
    RoomData[i] := Room;
    
    Room.RoomName[50] := #0;
    CleanName := StrPas(Room.RoomName);
    
    DisplayText := DisplayText + IntToStr(i) + '. Room ' + IntToStr(Room.RoomID) + 
                  ': ' + CleanName + #13;
  end;

  Close(F);

  { Create room selection dialog }
  R.Assign(10, 5, 70, 20);
  Dialog := New(PDialog, Init(R, 'Edit Room'));

  with Dialog^ do
  begin
    { Display room list }
    R.Assign(2, 2, 56, 12);
    ListText := New(PStaticText, Init(R, DisplayText));
    Insert(ListText);

    { Input field for room number }
    R.Assign(2, 13, 35, 14);
    Insert(New(PStaticText, Init(R, 'Enter room number to edit:')));
    
    R.Assign(36, 13, 46, 14);
    InputField := New(PInputLine, Init(R, 5));
    Insert(InputField);

    { Buttons }
    R.Assign(15, 15, 25, 17);
    Insert(New(PButton, Init(R, '~E~dit', cmOK, bfDefault)));

    R.Assign(30, 15, 40, 17);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  end;

  SelectedRoom := Desktop^.ExecView(Dialog);

  if SelectedRoom = cmOK then
  begin
    { Get the room number from input }
    RoomNumber := '';
    if InputField^.Data <> nil then
      RoomNumber := PShortString(InputField^.Data)^;
    
    SelectedIndex := StrToIntDef(RoomNumber, 0);
    
    { Validate room number }
    if (SelectedIndex < 1) or (SelectedIndex > RoomCount) then
    begin
      MessageBox('Invalid room number! Please enter a number between 1 and ' + 
                 IntToStr(RoomCount), nil, mfError + mfOKButton);
    end
    else
    begin
      { Get the selected room }
      Room := RoomData[SelectedIndex];
      
      { Ensure null termination }
      Room.RoomName[50] := #0;
      Room.Description[200] := #0;
      
      { Edit the room }
      EditRoomForm(Room);
      
      { Save changes back to file }
      UpdateRoomInFile(Room, SelectedIndex);
    end;
  end;

  Dispose(Dialog, Done);
end;

procedure ListRooms;
var
  F: File;
  Header: TRoomFileHeader;
  Room: TRoomRecord;
  Dialog: PDialog;
  R: TRect;
  i: Integer;
  SelectedRoom: Integer;
  CleanName: string;
  RoomData: array[1..50] of TRoomRecord;
  RoomCount: Integer;
  ListText: PStaticText;
  DisplayText: string;
  InputField: PInputLine;
  RoomNumber: string;
  SelectedIndex: Integer;
begin
  if not FileExists(ROOM_FILE_NAME) then
  begin
    MessageBox('No room file found!', nil, mfError + mfOKButton);
    Exit;
  end;

  Assign(F, ROOM_FILE_NAME);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageBox('Error opening room file!', nil, mfError + mfOKButton);
    Exit;
  end;

  { Read header }
  BlockRead(F, Header, SizeOf(Header));
  
  if Header.RecordCount = 0 then
  begin
    Close(F);
    MessageBox('No rooms found in file!', nil, mfInformation + mfOKButton);
    Exit;
  end;

  RoomCount := Header.RecordCount;
  if RoomCount > 50 then RoomCount := 50;

  { Read all rooms into array }
