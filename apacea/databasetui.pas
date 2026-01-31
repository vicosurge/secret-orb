program DatabaseTUI;

{$IFDEF FPC}
  {$MODE TP}
{$ENDIF}

uses
  Crt, Dos;

const
  MAX_RECORDS = 1000;
  MAX_TABLES = 10;
  HEADER_SIZE = 512;
  MAGIC_SIGNATURE = 'PTDB';
  VERSION = 1;

type
  { Game table structures }
  TNPC = record
    ID: LongInt;
    Name: String[30];
    Description: String[100];
    Location: Integer;
    Active: Boolean;
    Padding: array[1..10] of Byte; { Alignment padding }
  end;

  { Updated TItem structure for database.pas }
  TItem = record
    ID: LongInt;
    Name: String[40];
    Description: String[100];
    Value: LongInt; { Store as cents to avoid floating point }
    Weight: Integer;
    CategoryID: Integer;
    Location: Integer;     { Room ID where item is located, 0 = not in world, -1 = player inventory }
    Takeable: Boolean;     { Can the item be picked up? }
    Active: Boolean;
    Padding: array[1..8] of Byte; { Adjusted padding }
  end;

  { Simple Room Structure }
  TRoom = record
    ID: LongInt;
    Name: String[32];
    Description: String[200];
    ShortDescription: String[50];
    NorthWay: Integer;
    SouthWay: Integer;
    EastWay: Integer;
    WestWay: Integer;
    UpWay: Integer;
    DownWay: Integer;
    Active: Boolean;
    Padding: array[1..10] of Byte;
  end;

  { For triggers/puzzles, consider adding this structure }
  TTrigger = record
    ID: LongInt;
    Name: String[30];
    TriggerType: Integer;  { 1=Item pickup, 2=Room enter, 3=NPC talk, 4=Item use }
    SourceID: Integer;     { ID of item/room/NPC that triggers }
    TargetID: Integer;     { ID of what changes }
    ActionType: Integer;   { 1=Unlock exit, 2=Spawn item, 3=Change description, 4=Add score }
    Message: String[200];  { Message to display when triggered }
    OneTime: Boolean;      { Trigger only once? }
    Triggered: Boolean;    { Has it been triggered? }
    Active: Boolean;
    Padding: array[1..10] of Byte;
  end;

  TFileHeader = record
    Signature: array[1..4] of Char;
    Version: Integer;
    TableCount: Integer;
    NPCTableOffset: LongInt;
    NPCRecordSize: Integer;
    NPCRecordCount: LongInt;
    ItemTableOffset: LongInt;
    ItemRecordSize: Integer;
    ItemRecordCount: LongInt;
    RoomTableOffset: LongInt;
    RoomRecordSize: Integer;
    RoomRecordCount: LongInt;
    Reserved: array[1..460] of Byte; { Pad to 512 bytes }
  end;

var
  DatabaseFile: File;
  DatabaseFileName: String;
  FileHeader: TFileHeader;
  CurrentTable: Integer;
  CurrentRecord: LongInt;
  TotalRecords: LongInt;

{ Configuration from INI }
procedure LoadConfig;
var
  ConfigFile: Text;
  Line: String;
begin
  DatabaseFileName := 'database.dat'; { Default }

  Assign(ConfigFile, 'config.ini');
  {$I-}
  Reset(ConfigFile);
  {$I+}

  if IOResult = 0 then
  begin
    while not EOF(ConfigFile) do
    begin
      ReadLn(ConfigFile, Line);
      if Pos('DATABASE=', UpCase(Line)) = 1 then
        DatabaseFileName := Copy(Line, 10, Length(Line) - 9);
    end;
    Close(ConfigFile);
  end;
end;

{ Initialize or create database file }
procedure InitializeDatabase;
begin
  Assign(DatabaseFile, DatabaseFileName);

  { Try to open existing file }
  {$I-}
  Reset(DatabaseFile, 1);
  {$I+}

  if IOResult <> 0 then
  begin
    { Create new database }
    Rewrite(DatabaseFile, 1);

    { Initialize header }
    FillChar(FileHeader, SizeOf(FileHeader), 0);
    FileHeader.Signature := MAGIC_SIGNATURE;
    FileHeader.Version := VERSION;
    FileHeader.TableCount := 3;

    { NPC table setup }
    FileHeader.NPCTableOffset := HEADER_SIZE;
    FileHeader.NPCRecordSize := SizeOf(TNPC);
    FileHeader.NPCRecordCount := 0;

    { Item table setup }
    FileHeader.ItemTableOffset := HEADER_SIZE + (MAX_RECORDS * SizeOf(TNPC));
    FileHeader.ItemRecordSize := SizeOf(TItem);
    FileHeader.ItemRecordCount := 0;

    { Room table setup }
    FileHeader.RoomTableOffset := HEADER_SIZE + (MAX_RECORDS * SizeOf(TNPC)) + (MAX_RECORDS * SizeOf(TItem));
    FileHeader.RoomRecordSize := SizeOf(TRoom);
    FileHeader.RoomRecordCount := 0;

    { Write header }
    BlockWrite(DatabaseFile, FileHeader, SizeOf(FileHeader));

    WriteLn('Created new database: ', DatabaseFileName);
  end
  else
  begin
    { Read existing header }
    BlockRead(DatabaseFile, FileHeader, SizeOf(FileHeader));

    { Verify signature }
    if FileHeader.Signature <> MAGIC_SIGNATURE then
    begin
      WriteLn('Error: Invalid database file format');
      Halt(1);
    end;

    WriteLn('Opened existing database: ', DatabaseFileName);
  end;
end;

{ Update header in file }
procedure UpdateHeader;
begin
  Seek(DatabaseFile, 0);
  BlockWrite(DatabaseFile, FileHeader, SizeOf(FileHeader));
end;

{ Display main menu }
procedure DrawMainMenu;
begin
  ClrScr;
  GotoXY(1, 1);
  WriteLn('=== Text Adventure Database Manager ===');
  WriteLn('File: ', DatabaseFileName);
  WriteLn;
  WriteLn('1. Manage NPCs (', FileHeader.NPCRecordCount, ' records)');
  WriteLn('2. Manage Items (', FileHeader.ItemRecordCount, ' records)');
  WriteLn('3. Manage Rooms (', FileHeader.RoomRecordCount, ' records)');
  WriteLn('4. Exit');
  WriteLn;
  Write('Select option: ');
end;

{ Display NPC record }
procedure DisplayNPC(var NPC: TNPC; RecordNum: LongInt);
begin
  WriteLn('=== NPC Record ', RecordNum + 1, ' of ', FileHeader.NPCRecordCount, ' ===');
  if not NPC.Active then WriteLn('*** INACTIVE NPC ***');
  WriteLn('ID: ', NPC.ID);
  WriteLn('Name: ', NPC.Name);
  WriteLn('Description: ', NPC.Description);
  WriteLn('Location (Room ID): ', NPC.Location);
  Write('Active: ');
  if NPC.Active then WriteLn('Yes') else WriteLn('No');
  WriteLn;
end;

{ Updated DisplayItem procedure }
procedure DisplayItem(var Item: TItem; RecordNum: LongInt);
var
  LocationStr: String;
begin
  WriteLn('=== Item Record ', RecordNum + 1, ' of ', FileHeader.ItemRecordCount, ' ===');
  if not Item.Active then WriteLn('*** INACTIVE ITEM ***');
  WriteLn('ID: ', Item.ID);
  WriteLn('Name: ', Item.Name);
  WriteLn('Description: ', Item.Description);
  WriteLn('Value: , Item.Value div 100, '.', Item.Value mod 100:2);
  WriteLn('Weight: ', Item.Weight);
  WriteLn('Category ID: ', Item.CategoryID);

  { Display location in readable format }
  Write('Location: ');
  case Item.Location of
    -1: WriteLn('Player Inventory');
    0: WriteLn('Not in world');
  else
    WriteLn('Room ', Item.Location);
  end;

  Write('Takeable: ');
  if Item.Takeable then WriteLn('Yes') else WriteLn('No');
  Write('Active: ');
  if Item.Active then WriteLn('Yes') else WriteLn('No');
  WriteLn;
end;

{ Display room record }
procedure DisplayRoom(var Room: TRoom; RecordNum: LongInt);
begin
  WriteLn('=== Room Record ', RecordNum + 1, ' of ', FileHeader.RoomRecordCount, ' ===');
  if not Room.Active then WriteLn('*** INACTIVE ROOM ***');
  WriteLn('ID: ', Room.ID);
  WriteLn('Name: ', Room.Name);
  WriteLn('Description: ', Room.Description);
  WriteLn('Short Description: ', Room.ShortDescription);
  WriteLn('North Exit: ', Room.NorthWay);
  WriteLn('South Exit: ', Room.SouthWay);
  WriteLn('East Exit: ', Room.EastWay);
  WriteLn('West Exit: ', Room.WestWay);
  WriteLn('Up Exit: ', Room.UpWay);
  WriteLn('Down Exit: ', Room.DownWay);
  Write('Active: ');
  if Room.Active then WriteLn('Yes') else WriteLn('No');
  WriteLn;
end;

{ Read NPC record }
procedure ReadNPC(RecordNum: LongInt; var NPC: TNPC);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.NPCTableOffset + (RecordNum * FileHeader.NPCRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockRead(DatabaseFile, NPC, FileHeader.NPCRecordSize);
end;

{ Write NPC record }
procedure WriteNPC(RecordNum: LongInt; var NPC: TNPC);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.NPCTableOffset + (RecordNum * FileHeader.NPCRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockWrite(DatabaseFile, NPC, FileHeader.NPCRecordSize);
end;

{ Read item record }
procedure ReadItem(RecordNum: LongInt; var Item: TItem);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.ItemTableOffset + (RecordNum * FileHeader.ItemRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockRead(DatabaseFile, Item, FileHeader.ItemRecordSize);
end;

{ Write item record }
procedure WriteItem(RecordNum: LongInt; var Item: TItem);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.ItemTableOffset + (RecordNum * FileHeader.ItemRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockWrite(DatabaseFile, Item, FileHeader.ItemRecordSize);
end;

{ Read room record }
procedure ReadRoom(RecordNum: LongInt; var Room: TRoom);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.RoomTableOffset + (RecordNum * FileHeader.RoomRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockRead(DatabaseFile, Room, FileHeader.RoomRecordSize);
end;

{ Write room record }
procedure WriteRoom(RecordNum: LongInt; var Room: TRoom);
var
  FilePos: LongInt;
begin
  FilePos := FileHeader.RoomTableOffset + (RecordNum * FileHeader.RoomRecordSize);
  Seek(DatabaseFile, FilePos);
  BlockWrite(DatabaseFile, Room, FileHeader.RoomRecordSize);
end;

{ Add new NPC }
procedure AddNPC;
var
  NPC: TNPC;
  ActiveStr: String;
begin
  ClrScr;
  WriteLn('=== Add New NPC ===');

  FillChar(NPC, SizeOf(NPC), 0);

  { Get next ID }
  NPC.ID := FileHeader.NPCRecordCount + 1;

  Write('Name: '); ReadLn(NPC.Name);
  Write('Description: '); ReadLn(NPC.Description);
  Write('Location (Room ID): '); ReadLn(NPC.Location);
  Write('Active (Y/N): '); ReadLn(ActiveStr);
  NPC.Active := (UpCase(ActiveStr[1]) = 'Y');

  { Write record }
  WriteNPC(FileHeader.NPCRecordCount, NPC);
  Inc(FileHeader.NPCRecordCount);
  UpdateHeader;

  WriteLn('NPC added successfully!');
  WriteLn('Press any key to continue...');
  ReadKey;
end;

{ Updated AddItem procedure }
procedure AddItem;
var
  Item: TItem;
  ValueStr, ActiveStr, TakeableStr: String;
  ValueCents: Real;
  Code: Integer;
begin
  ClrScr;
  WriteLn('=== Add New Item ===');

  FillChar(Item, SizeOf(Item), 0);

  { Get next ID }
  Item.ID := FileHeader.ItemRecordCount + 1;

  Write('Name: '); ReadLn(Item.Name);
  Write('Description: '); ReadLn(Item.Description);
  Write('Value (dollars.cents): '); ReadLn(ValueStr);

  { Convert value to cents }
  Val(ValueStr, ValueCents, Code);
  if Code = 0 then
    Item.Value := Trunc(ValueCents * 100)
  else
    Item.Value := 0;

  Write('Weight: '); ReadLn(Item.Weight);
  Write('Category ID: '); ReadLn(Item.CategoryID);
  Write('Starting Location (Room ID, 0=nowhere, -1=inventory): ');
  ReadLn(Item.Location);
  Write('Can be taken? (Y/N): '); ReadLn(TakeableStr);
  Item.Takeable := (UpCase(TakeableStr[1]) = 'Y');
  Write('Active (Y/N): '); ReadLn(ActiveStr);
  Item.Active := (UpCase(ActiveStr[1]) = 'Y');

  { Write record }
  WriteItem(FileHeader.ItemRecordCount, Item);
  Inc(FileHeader.ItemRecordCount);
  UpdateHeader;

  WriteLn('Item added successfully!');
  WriteLn('Press any key to continue...');
  ReadKey;
end;

{ Updated EditItem procedure }
procedure EditItem(RecordNum: LongInt; var Item: TItem);
var
  Choice: Char;
  TempStr: String;
  TempVal: Real;
  TempInt: Integer;
  Changed: Boolean;
  Code: Integer;
begin
  Changed := False;

  repeat
    ClrScr;
    WriteLn('=== Edit Item Record ', RecordNum + 1, ' ===');
    WriteLn('1. Name: ', Item.Name);
    WriteLn('2. Description: ', Item.Description);
    WriteLn('3. Value: $', Item.Value div 100, '.', Item.Value mod 100:2);
    WriteLn('4. Weight: ', Item.Weight);
    WriteLn('5. Category ID: ', Item.CategoryID);
    WriteLn('6. Location: ', Item.Location, ' (0=nowhere, -1=inventory)');
    Write('7. Takeable: ');
    if Item.Takeable then WriteLn('Yes') else WriteLn('No');
    Write('8. Active: ');
    if Item.Active then WriteLn('Yes') else WriteLn('No');
    WriteLn;
    WriteLn('S. Save changes and return');
    WriteLn('ESC. Cancel without saving');
    Write('Select field to edit: ');

    Choice := ReadKey;

    case Choice of
      '1': begin
        Write('New Name: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Name := TempStr;
          Changed := True;
        end;
      end;
      '2': begin
        Write('New Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write('New Value (dollars.cents): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal, Code);
          if Code = 0 then
          begin
            Item.Value := Trunc(TempVal * 100);
            Changed := True;
          end;
        end;
      end;
      '4': begin
        Write('New Weight: '); ReadLn(TempInt);
        Item.Weight := TempInt;
        Changed := True;
      end;
      '5': begin
        Write('New Category ID: '); ReadLn(TempInt);
        Item.CategoryID := TempInt;
        Changed := True;
      end;
      '6': begin
        Write('New Location (Room ID, 0=nowhere, -1=inventory): ');
        ReadLn(TempInt);
        Item.Location := TempInt;
        Changed := True;
      end;
      '7': begin
        Write('Takeable (Y/N): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Takeable := (UpCase(TempStr[1]) = 'Y');
          Changed := True;
        end;
      end;
      '8': begin
        Write('Active (Y/N): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Active := (UpCase(TempStr[1]) = 'Y');
          Changed := True;
        end;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteItem(RecordNum, Item);
          WriteLn('Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;

{ Add new room }
procedure AddRoom;
var
  Room: TRoom;
  ActiveStr: String;
begin
  ClrScr;
  WriteLn('=== Add New Room ===');

  FillChar(Room, SizeOf(Room), 0);

  { Get next ID }
  Room.ID := FileHeader.RoomRecordCount + 1;

  Write('Name: '); ReadLn(Room.Name);
  Write('Description: '); ReadLn(Room.Description);
  Write('Short Description: '); ReadLn(Room.ShortDescription);
  Write('Active (Y/N): '); ReadLn(ActiveStr);
  Room.Active := (UpCase(ActiveStr[1]) = 'Y');

  { Write record }
  WriteRoom(FileHeader.RoomRecordCount, Room);
  Inc(FileHeader.RoomRecordCount);
  UpdateHeader;

  WriteLn('Room added successfully!');
  WriteLn('Press any key to continue...');
  ReadKey;
end;

{ Edit NPC }
procedure EditNPC(RecordNum: LongInt; var NPC: TNPC);
var
  Choice: Char;
  TempStr: String;
  TempVal: LongInt;
  Changed: Boolean;
begin
  Changed := False;

  repeat
    ClrScr;
    WriteLn('=== Edit NPC Record ', RecordNum + 1, ' ===');
    WriteLn('1. Name: ', NPC.Name);
    WriteLn('2. Description: ', NPC.Description);
    WriteLn('3. Location (Room ID): ', NPC.Location);
    Write('4. Active: ');
    if NPC.Active then WriteLn('Yes') else WriteLn('No');
    WriteLn;
    WriteLn('S. Save changes and return');
    WriteLn('ESC. Cancel without saving');
    Write('Select field to edit: ');

    Choice := ReadKey;

    case Choice of
      '1': begin
        Write(#13#10+'New Name: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          NPC.Name := TempStr;
          Changed := True;
        end;
      end;
      '2': begin
        Write(#13#10+'New Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          NPC.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write(#13#10+'New Location (Room ID): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal);
          NPC.Location := TempVal;
          Changed := True;
        end;
      end;
      '4': begin
        Write(#13#10+'Active (Y/N): '); ReadLn(TempStr);
        NPC.Active := (UpCase(TempStr[1]) = 'Y');
        Changed := True;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteNPC(RecordNum, NPC);
          WriteLn(#13#10+'Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;

{ Edit item }
procedure EditItem(RecordNum: LongInt; var Item: TItem);
var
  Choice: Char;
  TempStr: String;
  TempVal: Real;
  TempInt: Integer;
  Changed: Boolean;
begin
  Changed := False;

  repeat
    ClrScr;
    WriteLn('=== Edit Item Record ', RecordNum + 1, ' ===');
    WriteLn('1. Name: ', Item.Name);
    WriteLn('2. Description: ', Item.Description);
    WriteLn('3. Value: $', Item.Value div 100, '.', Item.Value mod 100:2);
    WriteLn('4. Weight: ', Item.Weight);
    WriteLn('5. Category ID: ', Item.CategoryID);
    Write('6. Active: ');
    if Item.Active then WriteLn('Yes') else WriteLn('No');
    WriteLn;
    WriteLn('S. Save changes and return');
    WriteLn('ESC. Cancel without saving');
    Write('Select field to edit: ');

    Choice := ReadKey;

    case Choice of
      '1': begin
        Write(#13#10+'New Name: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Name := TempStr;
          Changed := True;
        end;
      end;
      '2': begin
        Write(#13#10+'New Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write(#13#10+'New Value (dollars.cents): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal, TempInt);
          if TempInt = 0 then
          begin
            Item.Value := Trunc(TempVal * 100);
            Changed := True;
          end;
        end;
      end;
      '4': begin
        Write(#13#10+'New Weight: '); ReadLn(TempInt);
        Item.Weight := TempInt;
        Changed := True;
      end;
      '5': begin
        Write(#13#10+'New Category ID: '); ReadLn(TempInt);
        Item.CategoryID := TempInt;
        Changed := True;
      end;
      '6': begin
        Write(#13#10+'Active (Y/N): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Active := (UpCase(TempStr[1]) = 'Y');
          Changed := True;
        end;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteItem(RecordNum, Item);
          WriteLn(#13#10+'Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;

{ Edit room }
procedure EditRoom(RecordNum: LongInt; var Room: TRoom);
var
  Choice: Char;
  TempStr: String;
  TempInt: Integer;
  Changed: Boolean;
begin
  Changed := False;

  repeat
    ClrScr;
    WriteLn('=== Edit Room Record ', RecordNum + 1, ' ===');
    WriteLn('1. Name: ', Room.Name);
    WriteLn('2. Description: ', Room.Description);
    WriteLn('3. Short Description: ', Room.ShortDescription);
    WriteLn('4. North Exit (Room ID): ', Room.NorthWay);
    WriteLn('5. South Exit (Room ID): ', Room.SouthWay);
    WriteLn('6. East Exit (Room ID): ', Room.EastWay);
    WriteLn('7. West Exit (Room ID): ', Room.WestWay);
    WriteLn('8. Up Exit (Room ID): ', Room.UpWay);
    WriteLn('9. Down Exit (Room ID): ', Room.DownWay);
    Write('A. Active: ');
    if Room.Active then WriteLn('Yes') else WriteLn('No');
    WriteLn;
    WriteLn('S. Save changes and return');
    WriteLn('ESC. Cancel without saving');
    Write('Select field to edit: ');

    Choice := ReadKey;

    case Choice of
      '1': begin
        Write(#13#10+'New Name: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Room.Name := TempStr;
          Changed := True;
        end;
      end;
      '2': begin
        Write(#13#10+'New Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Room.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write(#13#10+'New Short Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Room.ShortDescription := TempStr;
          Changed := True;
        end;
      end;
      '4': begin
        Write(#13#10+'North Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.NorthWay := TempInt;
        Changed := True;
      end;
      '5': begin
        Write(#13#10+'South Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.SouthWay := TempInt;
        Changed := True;
      end;
      '6': begin
        Write(#13#10+'East Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.EastWay := TempInt;
        Changed := True;
      end;
      '7': begin
        Write(#13#10+'West Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.WestWay := TempInt;
        Changed := True;
      end;
      '8': begin
        Write(#13#10+'Up Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.UpWay := TempInt;
        Changed := True;
      end;
      '9': begin
        Write(#13#10+'Down Exit (Room ID, 0 for none): '); ReadLn(TempInt);
        Room.DownWay := TempInt;
        Changed := True;
      end;
      'A', 'a': begin
        Write('Active (Y/N): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Room.Active := (UpCase(TempStr[1]) = 'Y');
          Changed := True;
        end;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteRoom(RecordNum, Room);
          WriteLn(#13#10+'Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;

{ Browse and manage NPCs }
procedure ManageNPCs;
var
  NPC: TNPC;
  Key: Char;
  CurrentRec: LongInt;
  Choice: Char;
begin
  if FileHeader.NPCRecordCount = 0 then
  begin
    ClrScr;
    WriteLn('No NPCs in database.');
    WriteLn;
    WriteLn('A. Add new NPC');
    WriteLn('ESC. Return to main menu');
    Key := ReadKey;
    if UpCase(Key) = 'A' then AddNPC;
    Exit;
  end;

  CurrentRec := 0;

  repeat
    ClrScr;
    ReadNPC(CurrentRec, NPC);
    DisplayNPC(NPC, CurrentRec);

    WriteLn('Navigation: ← → (arrows), PgUp/PgDn');
    WriteLn('Actions: A)dd, E)dit, D)eactivate/Activate, ESC)ape');

    Key := ReadKey;

    case Key of
      #0: begin { Extended key }
        Key := ReadKey;
        case Key of
          #75: if CurrentRec > 0 then Dec(CurrentRec); { Left arrow }
          #77: if CurrentRec < FileHeader.NPCRecordCount - 1 then Inc(CurrentRec); { Right arrow }
          #73: if CurrentRec > 0 then Dec(CurrentRec); { PgUp }
          #81: if CurrentRec < FileHeader.NPCRecordCount - 1 then Inc(CurrentRec); { PgDn }
        end;
      end;
      'A', 'a': AddNPC;
      'E', 'e': EditNPC(CurrentRec, NPC);
      'D', 'd': begin
        if NPC.Active then
        begin
          Write('Deactivate this NPC? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            NPC.Active := False;
            WriteNPC(CurrentRec, NPC);
            WriteLn('NPC deactivated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end else
        begin
          Write('Activate this NPC? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            NPC.Active := True;
            WriteNPC(CurrentRec, NPC);
            WriteLn('NPC activated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end;
      end;
      #27: Break; { ESC }
    end;

  until False;
end;

{ Browse and manage items }
procedure ManageItems;
var
  Item: TItem;
  Key: Char;
  CurrentRec: LongInt;
  Choice: Char;
begin
  if FileHeader.ItemRecordCount = 0 then
  begin
    ClrScr;
    WriteLn('No items in database.');
    WriteLn;
    WriteLn('A. Add new item');
    WriteLn('ESC. Return to main menu');
    Key := ReadKey;
    if UpCase(Key) = 'A' then AddItem;
    Exit;
  end;

  CurrentRec := 0;

  repeat
    ClrScr;
    ReadItem(CurrentRec, Item);
    DisplayItem(Item, CurrentRec);

    WriteLn('Navigation: ← → (arrows), PgUp/PgDn');
    WriteLn('Actions: A)dd, E)dit, D)eactivate/Activate, ESC)ape');

    Key := ReadKey;

    case Key of
      #0: begin { Extended key }
        Key := ReadKey;
        case Key of
          #75: if CurrentRec > 0 then Dec(CurrentRec); { Left arrow }
          #77: if CurrentRec < FileHeader.ItemRecordCount - 1 then Inc(CurrentRec); { Right arrow }
          #73: if CurrentRec > 0 then Dec(CurrentRec); { PgUp }
          #81: if CurrentRec < FileHeader.ItemRecordCount - 1 then Inc(CurrentRec); { PgDn }
        end;
      end;
      'A', 'a': AddItem;
      'E', 'e': EditItem(CurrentRec, Item);
      'D', 'd': begin
        if Item.Active then
        begin
          Write('Deactivate this item? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            Item.Active := False;
            WriteItem(CurrentRec, Item);
            WriteLn('Item deactivated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end else
        begin
          Write('Activate this item? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            Item.Active := True;
            WriteItem(CurrentRec, Item);
            WriteLn('Item activated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end;
      end;
      #27: Break; { ESC }
    end;

  until False;
end;

{ Browse and manage rooms }
procedure ManageRooms;
var
  Room: TRoom;
  Key: Char;
  CurrentRec: LongInt;
  Choice: Char;
begin
  if FileHeader.RoomRecordCount = 0 then
  begin
    ClrScr;
    WriteLn('No rooms in database.');
    WriteLn;
    WriteLn('A. Add new room');
    WriteLn('ESC. Return to main menu');
    Key := ReadKey;
    if UpCase(Key) = 'A' then AddRoom;
    Exit;
  end;

  CurrentRec := 0;

  repeat
    ClrScr;
    ReadRoom(CurrentRec, Room);
    DisplayRoom(Room, CurrentRec);

    WriteLn('Navigation: ← → (arrows), PgUp/PgDn');
    WriteLn('Actions: A)dd, E)dit, D)eactivate/Activate, ESC)ape');

    Key := ReadKey;

    case Key of
      #0: begin { Extended key }
        Key := ReadKey;
        case Key of
          #75: if CurrentRec > 0 then Dec(CurrentRec); { Left arrow }
          #77: if CurrentRec < FileHeader.RoomRecordCount - 1 then Inc(CurrentRec); { Right arrow }
          #73: if CurrentRec > 0 then Dec(CurrentRec); { PgUp }
          #81: if CurrentRec < FileHeader.RoomRecordCount - 1 then Inc(CurrentRec); { PgDn }
        end;
      end;
      'A', 'a': AddRoom;
      'E', 'e': begin
        EditRoom(CurrentRec, Room);
        ReadRoom(CurrentRec, Room); { Re-read after editing }
      end;
      'D', 'd': begin
        if Room.Active then
        begin
          Write('Deactivate this room? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            Room.Active := False;
            WriteRoom(CurrentRec, Room);
            WriteLn('Room deactivated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end else
        begin
          Write('Activate this room? (Y/N): ');
          Choice := ReadKey;
          WriteLn(Choice);
          if UpCase(Choice) = 'Y' then
          begin
            Room.Active := True;
            WriteRoom(CurrentRec, Room);
            WriteLn('Room activated.');
            WriteLn('Press any key...');
            ReadKey;
          end;
        end;
      end;
      #27: Break; { ESC }
    end;

  until False;
end;

{ Main program }
var
  Choice: Char;

begin
  { Initialize }
  LoadConfig;
  InitializeDatabase;

  { Main loop }
  repeat
    DrawMainMenu;
    Choice := ReadKey;

    case Choice of
      '1': ManageNPCs;
      '2': ManageItems;
      '3': ManageRooms;
      '4', #27: Break; { Exit or ESC }
    end;

  until False;

  { Cleanup }
  Close(DatabaseFile);
  WriteLn('Database closed. Goodbye!');
end.(TempStr);
        if TempStr <> '' then
        begin
          NPC.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write('New Location (Room ID): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal);
          NPC.Location := TempVal;
          Changed := True;
        end;
      end;
      '4': begin
        Write('Active (Y/N): '); ReadLn(TempStr);
        NPC.Active := (UpCase(TempStr[1]) = 'Y');
        Changed := True;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteNPC(RecordNum, NPC);
          WriteLn('Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;

{ Edit item }
procedure EditItem(RecordNum: LongInt; var Item: TItem);
var
  Choice: Char;
  TempStr: String;
  TempVal: LongInt;
  Changed: Boolean;
begin
  Changed := False;

  repeat
    ClrScr;
    WriteLn('=== Edit Item Record ', RecordNum + 1, ' ===');
    WriteLn('1. Name: ', Item.Name);
    WriteLn('2. Description: ', Item.Description);
    WriteLn('3. Value: $', Item.Value div 100, '.', Item.Value mod 100:2);
    WriteLn('4. Weight: ', Item.Weight);
    WriteLn('5. Category ID: ', Item.CategoryID);
    Write('6. Active: ');
    if Item.Active then WriteLn('Yes') else WriteLn('No');
    WriteLn;
    WriteLn('S. Save changes and return');
    WriteLn('ESC. Cancel without saving');
    Write('Select field to edit: ');

    Choice := ReadKey;

    case Choice of
      '1': begin
        Write('New Name: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Name := TempStr;
          Changed := True;
        end;
      end;
      '2': begin
        Write('New Description: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Item.Description := TempStr;
          Changed := True;
        end;
      end;
      '3': begin
        Write('New Value (dollars.cents): '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal);
          if Pos('.', TempStr) > 0 then
            Item.Value := Trunc(TempVal * 100)
          else
            Item.Value := TempVal * 100;
          Changed := True;
        end;
      end;
      '4': begin
        Write('New Weight: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal);
          Item.Weight := TempVal;
          Changed := True;
        end;
      end;
      '5': begin
        Write('New Category ID: '); ReadLn(TempStr);
        if TempStr <> '' then
        begin
          Val(TempStr, TempVal);
          Item.CategoryID := TempVal;
          Changed := True;
        end;
      end;
      '6': begin
        Write('Active (Y/N): '); ReadLn(TempStr);
        Item.Active := (UpCase(TempStr[1]) = 'Y');
        Changed := True;
      end;
      'S', 's': begin
        if Changed then
        begin
          WriteItem(RecordNum, Item);
          WriteLn('Changes saved!');
        end else
          WriteLn('No changes made.');
        WriteLn('Press any key...');
        ReadKey;
        Break;
      end;
      #27: Break; { ESC - cancel }
    end;

  until False;
end;
