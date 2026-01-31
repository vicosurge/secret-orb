program AdventureEngine;

{$IFDEF FPC}
  {$MODE TP}
{$ENDIF}

uses
  Crt, Dos;

const
  MAX_RECORDS = 1000;
  HEADER_SIZE = 512;
  MAGIC_SIGNATURE = 'PTDB';
  MAX_INVENTORY = 50;
  SAVE_SIGNATURE = 'SAVE';
  SAVE_VERSION = 1;
  MAX_TRIGGERS = 100;

type
  { Game table structures - must match database }
  TNPC = record
    ID: LongInt;
    Name: String[30];
    Description: String[100];
    Location: Integer;
    Active: Boolean;
    Padding: array[1..10] of Byte;
  end;

  TItem = record
    ID: LongInt;
    Name: String[40];
    Description: String[100];
    Value: LongInt;
    Weight: Integer;
    CategoryID: Integer;
    Location: Integer;     { Room ID, 0 = not in world, -1 = player inventory }
    Takeable: Boolean;     { Can be picked up? }
    Active: Boolean;
    Padding: array[1..8] of Byte;
  end;

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
    Reserved: array[1..460] of Byte;
  end;

  { Save game structure }
  TSaveGame = record
    Signature: array[1..4] of Char;
    Version: Integer;
    SaveDate: String[20];
    CurrentRoom: Integer;
    Score: Integer;
    Moves: Integer;
    { Item locations will be saved separately }
  end;

  { Player state }
  TPlayer = record
    CurrentRoom: Integer;
    Score: Integer;
    Moves: Integer;
  end;

  { Trigger types }
  TTriggerType = (
    trgItemPickup,    { Triggered when item is picked up }
    trgItemDrop,      { Triggered when item is dropped }
    trgItemUse,       { Triggered when item is used }
    trgRoomEnter,     { Triggered when entering room }
    trgRoomExit,      { Triggered when leaving room }
    trgNPCTalk,       { Triggered when talking to NPC }
    trgScore,         { Triggered when score reaches value }
    trgCombination    { Triggered by item combination }
  );

  { Action types }
  TActionType = (
    actUnlockExit,    { Opens a blocked exit }
    actBlockExit,     { Blocks an exit }
    actSpawnItem,     { Creates item in room }
    actRemoveItem,    { Removes item from game }
    actChangeDesc,    { Changes room/item description }
    actAddScore,      { Adds to score }
    actMoveNPC,       { Moves NPC to new location }
    actEndGame,       { Ends the game (win/lose) }
    actShowMessage,   { Just displays a message }
    actTeleport       { Moves player to new room }
  );

  { Trigger record }
  TTrigger = record
    ID: Integer;
    Name: String[30];
    TriggerType: TTriggerType;
    SourceID: Integer;      { ID of item/room/NPC that triggers }
    TargetID: Integer;      { ID of what changes }
    ActionType: TActionType;
    ActionValue: Integer;   { Value for action (score amount, new room, etc) }
    Message: String[200];   { Message to display }
    RequiredItem: Integer;  { Item needed in inventory (0 = none) }
    OneTime: Boolean;       { Trigger only once? }
    Triggered: Boolean;     { Has been triggered? }
    Active: Boolean;
  end;

  { Dynamic arrays for game data }
  PNPCArray = ^TNPCArray;
  TNPCArray = array[0..MAX_RECORDS-1] of TNPC;

  PItemArray = ^TItemArray;
  TItemArray = array[0..MAX_RECORDS-1] of TItem;

  PRoomArray = ^TRoomArray;
  TRoomArray = array[0..MAX_RECORDS-1] of TRoom;

var
  DatabaseFile: File;
  DatabaseFileName: String;
  FileHeader: TFileHeader;

  { Game data arrays }
  NPCs: PNPCArray;
  Items: PItemArray;
  Rooms: PRoomArray;

  { Player state }
  Player: TPlayer;

  { Game flags }
  GameRunning: Boolean;
  RoomVisited: array[1..MAX_RECORDS] of Boolean;

  { Triggers }
  Triggers: array[1..MAX_TRIGGERS] of TTrigger;
  TriggerCount: Integer;

{ Load configuration }
procedure LoadConfig;
var
  ConfigFile: Text;
  Line: String;
begin
  DatabaseFileName := 'database.dat';

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

{ Load database into memory }
function LoadDatabase: Boolean;
var
  i: Integer;
begin
  LoadDatabase := False;

  Assign(DatabaseFile, DatabaseFileName);
  {$I-}
  Reset(DatabaseFile, 1);
  {$I+}

  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open database file: ', DatabaseFileName);
    Exit;
  end;

  { Read header }
  BlockRead(DatabaseFile, FileHeader, SizeOf(FileHeader));

  { Verify signature }
  if FileHeader.Signature <> MAGIC_SIGNATURE then
  begin
    WriteLn('Error: Invalid database file format');
    Close(DatabaseFile);
    Exit;
  end;

  { Allocate memory }
  New(NPCs);
  New(Items);
  New(Rooms);

  { Load NPCs }
  if FileHeader.NPCRecordCount > 0 then
  begin
    Seek(DatabaseFile, FileHeader.NPCTableOffset);
    for i := 0 to FileHeader.NPCRecordCount - 1 do
    begin
      BlockRead(DatabaseFile, NPCs^[i], FileHeader.NPCRecordSize);
    end;
  end;

  { Load Items }
  if FileHeader.ItemRecordCount > 0 then
  begin
    Seek(DatabaseFile, FileHeader.ItemTableOffset);
    for i := 0 to FileHeader.ItemRecordCount - 1 do
    begin
      BlockRead(DatabaseFile, Items^[i], FileHeader.ItemRecordSize);
    end;
  end;

  { Load Rooms }
  if FileHeader.RoomRecordCount > 0 then
  begin
    Seek(DatabaseFile, FileHeader.RoomTableOffset);
    for i := 0 to FileHeader.RoomRecordCount - 1 do
    begin
      BlockRead(DatabaseFile, Rooms^[i], FileHeader.RoomRecordSize);
    end;
  end;

  Close(DatabaseFile);
  LoadDatabase := True;
end;

{ Save game state }
procedure SaveGame;
var
  SaveFile: File;
  SaveData: TSaveGame;
  i: Integer;
  Year, Month, Day, DayOfWeek: Word;
  Hour, Minute, Second, Sec100: Word;
begin
  FillChar(SaveData, SizeOf(SaveData), 0);
  SaveData.Signature := SAVE_SIGNATURE;
  SaveData.Version := SAVE_VERSION;

  { Get current date/time }
  GetDate(Year, Month, Day, DayOfWeek);
  GetTime(Hour, Minute, Second, Sec100);
  Str(Year:4, SaveData.SaveDate);
  SaveData.SaveDate := SaveData.SaveDate + '-';
  Str(Month:2, SaveData.SaveDate);
  SaveData.SaveDate := SaveData.SaveDate + '-';
  Str(Day:2, SaveData.SaveDate);

  SaveData.CurrentRoom := Player.CurrentRoom;
  SaveData.Score := Player.Score;
  SaveData.Moves := Player.Moves;

  Assign(SaveFile, 'savegame.sav');
  {$I-}
  Rewrite(SaveFile, 1);
  {$I+}

  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot create save file!');
    Exit;
  end;

  { Write save header }
  BlockWrite(SaveFile, SaveData, SizeOf(SaveData));

  { Write item locations }
  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    BlockWrite(SaveFile, Items^[i].Location, SizeOf(Integer));
  end;

  { Write visited rooms }
  BlockWrite(SaveFile, RoomVisited, SizeOf(RoomVisited));

  Close(SaveFile);
  WriteLn('Game saved successfully!');
end;

{ Load game state }
function LoadGame: Boolean;
var
  SaveFile: File;
  SaveData: TSaveGame;
  i: Integer;
begin
  LoadGame := False;

  Assign(SaveFile, 'savegame.sav');
  {$I-}
  Reset(SaveFile, 1);
  {$I+}

  if IOResult <> 0 then
  begin
    WriteLn('No save game found.');
    Exit;
  end;

  { Read save header }
  BlockRead(SaveFile, SaveData, SizeOf(SaveData));

  { Verify save file }
  if SaveData.Signature <> SAVE_SIGNATURE then
  begin
    WriteLn('Error: Invalid save file!');
    Close(SaveFile);
    Exit;
  end;

  Player.CurrentRoom := SaveData.CurrentRoom;
  Player.Score := SaveData.Score;
  Player.Moves := SaveData.Moves;

  { Read item locations }
  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    BlockRead(SaveFile, Items^[i].Location, SizeOf(Integer));
  end;

  { Read visited rooms }
  BlockRead(SaveFile, RoomVisited, SizeOf(RoomVisited));

  Close(SaveFile);

  WriteLn('Game loaded from ', SaveData.SaveDate);
  LoadGame := True;
end;

{ Initialize new game }
procedure InitNewGame;
var
  i: Integer;
begin
  FillChar(Player, SizeOf(Player), 0);
  Player.CurrentRoom := 1; { Start in room 1 }
  Player.Score := 0;
  Player.Moves := 0;

  FillChar(RoomVisited, SizeOf(RoomVisited), 0);

  { Reset item locations to their default from database }
  { Items with Location = -1 will need to be set to 0 or their starting room }
end;

{ Find room by ID }
function FindRoom(RoomID: Integer): Integer;
var
  i: Integer;
begin
  FindRoom := -1;
  for i := 0 to FileHeader.RoomRecordCount - 1 do
  begin
    if (Rooms^[i].ID = RoomID) and Rooms^[i].Active then
    begin
      FindRoom := i;
      Exit;
    end;
  end;
end;

{ Display current room }
procedure ShowRoom;
var
  RoomIndex: Integer;
  i: Integer;
  NPCFound, ItemFound: Boolean;
begin
  RoomIndex := FindRoom(Player.CurrentRoom);
  if RoomIndex < 0 then
  begin
    WriteLn('Error: Invalid room!');
    Exit;
  end;

  ClrScr;
  WriteLn('=== ', Rooms^[RoomIndex].Name, ' ===');
  WriteLn;

  { Show full description first time, short description on revisit }
  if not RoomVisited[Player.CurrentRoom] then
  begin
    WriteLn(Rooms^[RoomIndex].Description);
    RoomVisited[Player.CurrentRoom] := True;
  end
  else
    WriteLn(Rooms^[RoomIndex].ShortDescription);

  WriteLn;

  { Show exits }
  Write('Exits: ');
  if Rooms^[RoomIndex].NorthWay > 0 then Write('North ');
  if Rooms^[RoomIndex].SouthWay > 0 then Write('South ');
  if Rooms^[RoomIndex].EastWay > 0 then Write('East ');
  if Rooms^[RoomIndex].WestWay > 0 then Write('West ');
  if Rooms^[RoomIndex].UpWay > 0 then Write('Up ');
  if Rooms^[RoomIndex].DownWay > 0 then Write('Down ');
  WriteLn;

  { Show items in room }
  ItemFound := False;
  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Active and (Items^[i].Location = Player.CurrentRoom) then
    begin
      if not ItemFound then
      begin
        WriteLn;
        WriteLn('You can see:');
        ItemFound := True;
      end;
      WriteLn('  ', Items^[i].Name);
    end;
  end;

  { Show NPCs in room }
  NPCFound := False;
  for i := 0 to FileHeader.NPCRecordCount - 1 do
  begin
    if NPCs^[i].Active and (NPCs^[i].Location = Player.CurrentRoom) then
    begin
      if not NPCFound then
      begin
        WriteLn;
        WriteLn('Also here:');
        NPCFound := True;
      end;
      WriteLn('  ', NPCs^[i].Name, ' - ', NPCs^[i].Description);
    end;
  end;

  WriteLn;
  WriteLn('[Moves: ', Player.Moves, '  Score: ', Player.Score, ']');
  WriteLn;
end;

{ Move player }
procedure MovePlayer(Direction: String);
var
  RoomIndex, NewRoom: Integer;
begin
  RoomIndex := FindRoom(Player.CurrentRoom);
  if RoomIndex < 0 then Exit;

  NewRoom := 0;
  Direction := UpCase(Direction);

  if (Direction = 'NORTH') or (Direction = 'N') then NewRoom := Rooms^[RoomIndex].NorthWay
  else if (Direction = 'SOUTH') or (Direction = 'S') then NewRoom := Rooms^[RoomIndex].SouthWay
  else if (Direction = 'EAST') or (Direction = 'E') then NewRoom := Rooms^[RoomIndex].EastWay
  else if (Direction = 'WEST') or (Direction = 'W') then NewRoom := Rooms^[RoomIndex].WestWay
  else if (Direction = 'UP') or (Direction = 'U') then NewRoom := Rooms^[RoomIndex].UpWay
  else if (Direction = 'DOWN') or (Direction = 'D') then NewRoom := Rooms^[RoomIndex].DownWay;

  if NewRoom > 0 then
  begin
    if FindRoom(NewRoom) >= 0 then
    begin
      Player.CurrentRoom := NewRoom;
      Inc(Player.Moves);
      ShowRoom;
    end
    else
      WriteLn('You can''t go that way - the path is blocked!');
  end
  else
    WriteLn('You can''t go that way!');
end;

{ Show inventory }
procedure ShowInventory;
var
  i: Integer;
  ItemCount: Integer;
begin
  WriteLn('You are carrying:');
  ItemCount := 0;

  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Location = -1 then { -1 means in player inventory }
    begin
      WriteLn('  ', Items^[i].Name);
      Inc(ItemCount);
    end;
  end;

  if ItemCount = 0 then
    WriteLn('  Nothing.');
end;

{ Take item }
procedure TakeItem(ItemName: String);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  ItemName := UpCase(ItemName);

  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Active and
       (Items^[i].Location = Player.CurrentRoom) and
       (Pos(ItemName, UpCase(Items^[i].Name)) > 0) then
    begin
      Found := True;
      if Items^[i].Takeable then
      begin
        Items^[i].Location := -1; { Move to inventory }
        WriteLn('Taken: ', Items^[i].Name);
        Inc(Player.Score, 10);
      end
      else
        WriteLn('You can''t take that.');
      Break;
    end;
  end;

  if not Found then
    WriteLn('You don''t see that here.');
end;

{ Drop item }
procedure DropItem(ItemName: String);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  ItemName := UpCase(ItemName);

  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Active and
       (Items^[i].Location = -1) and
       (Pos(ItemName, UpCase(Items^[i].Name)) > 0) then
    begin
      Found := True;
      Items^[i].Location := Player.CurrentRoom;
      WriteLn('Dropped: ', Items^[i].Name);
      Break;
    end;
  end;

  if not Found then
    WriteLn('You''re not carrying that.');
end;

{ Parse and execute command }
procedure ParseCommand(Command: String);
var
  Verb, Noun: String;
  SpacePos: Integer;
begin
  { Split into verb and noun }
  SpacePos := Pos(' ', Command);
  if SpacePos > 0 then
  begin
    Verb := UpCase(Copy(Command, 1, SpacePos - 1));
    Noun := Copy(Command, SpacePos + 1, Length(Command) - SpacePos);
  end
  else
  begin
    Verb := UpCase(Command);
    Noun := '';
  end;

  { Process commands }
  if (Verb = 'QUIT') or (Verb = 'EXIT') or (Verb = 'Q') then
  begin
    Write('Save before quitting? (Y/N): ');
    if UpCase(ReadKey) = 'Y' then
      SaveGame;
    WriteLn;
    GameRunning := False;
  end
  else if (Verb = 'SAVE') then
    SaveGame
  else if (Verb = 'LOAD') then
  begin
    if LoadGame then
      ShowRoom;
  end
  else if (Verb = 'RESTART') then
  begin
    Write('Restart the game? (Y/N): ');
    if UpCase(ReadKey) = 'Y' then
    begin
      WriteLn;
      InitNewGame;
      ShowRoom;
    end;
  end
  else if (Verb = 'NORTH') or (Verb = 'N') then
    MovePlayer('NORTH')
  else if (Verb = 'SOUTH') or (Verb = 'S') then
    MovePlayer('SOUTH')
  else if (Verb = 'EAST') or (Verb = 'E') then
    MovePlayer('EAST')
  else if (Verb = 'WEST') or (Verb = 'W') then
    MovePlayer('WEST')
  else if (Verb = 'UP') or (Verb = 'U') then
    MovePlayer('UP')
  else if (Verb = 'DOWN') or (Verb = 'D') then
    MovePlayer('DOWN')
  else if (Verb = 'GO') and (Noun <> '') then
    MovePlayer(Noun)
  else if (Verb = 'LOOK') or (Verb = 'L') then
  begin
    RoomVisited[Player.CurrentRoom] := False; { Force full description }
    ShowRoom;
  end
  else if (Verb = 'INVENTORY') or (Verb = 'INV') or (Verb = 'I') then
    ShowInventory
  else if (Verb = 'GET') or (Verb = 'TAKE') then
  begin
    if Noun = '' then
      WriteLn('Take what?')
    else
      TakeItem(Noun);
  end
  else if (Verb = 'DROP') then
  begin
    if Noun = '' then
      WriteLn('Drop what?')
    else
      DropItem(Noun);
  end
  else if (Verb = 'EXAMINE') or (Verb = 'X') then
  begin
    if Noun = '' then
      WriteLn('Examine what?')
    else
    begin
      { Add detailed examine functionality here }
      WriteLn('You see nothing special about that.');
    end;
  end
  else if (Verb = 'HELP') or (Verb = '?') then
  begin
    WriteLn('Commands:');
    WriteLn('  Movement: N/S/E/W/U/D or GO <direction>');
    WriteLn('  LOOK/L - Describe location');
    WriteLn('  INVENTORY/I - Show items');
    WriteLn('  TAKE/GET <item> - Pick up');
    WriteLn('  DROP <item> - Drop item');
    WriteLn('  EXAMINE/X <item> - Look closely');
    WriteLn('  SAVE/LOAD - Save or load game');
    WriteLn('  RESTART - Start over');
    WriteLn('  QUIT/Q - Exit game');
  end
  else
    WriteLn('I don''t understand. Type HELP for commands.');
end;

{ Main game loop }
procedure GameLoop;
var
  InputStr: String;
begin
  GameRunning := True;
  ShowRoom;

  while GameRunning do
  begin
    Write('> ');
    ReadLn(InputStr);

    if InputStr <> '' then
      ParseCommand(InputStr);
  end;
end;

{ Initialize triggers - this would normally load from database }
procedure InitTriggers;
begin
  TriggerCount := 0;

  { Example: Pick up key unlocks door }
  Inc(TriggerCount);
  with Triggers[TriggerCount] do
  begin
    ID := 1;
    Name := 'Get Key';
    TriggerType := trgItemPickup;
    SourceID := 1;  { Key item ID }
    TargetID := 2;  { Room 2 }
    ActionType := actUnlockExit;
    ActionValue := 1; { North exit }
    Message := 'The key feels warm in your hand. You hear a click in the distance.';
    RequiredItem := 0;
    OneTime := True;
    Triggered := False;
    Active := True;
  end;

  { Example: Enter treasure room adds score }
  Inc(TriggerCount);
  with Triggers[TriggerCount] do
  begin
    ID := 2;
    Name := 'Find Treasure';
    TriggerType := trgRoomEnter;
    SourceID := 5;  { Treasure room ID }
    TargetID := 0;
    ActionType := actAddScore;
    ActionValue := 100;
    Message := 'You found the treasure room! Your score increases!';
    RequiredItem := 0;
    OneTime := True;
    Triggered := False;
    Active := True;
  end;
end;

{ Check and execute triggers }
procedure CheckTriggers(TrigType: TTriggerType; SourceID: Integer);
var
  i: Integer;
  RoomIndex: Integer;
begin
  for i := 1 to TriggerCount do
  begin
    with Triggers[i] do
    begin
      if Active and (not Triggered or not OneTime) and
         (TriggerType = TrigType) and (SourceID = SourceID) then
      begin
        { Check if required item is in inventory }
        if RequiredItem > 0 then
        begin
          { Check inventory for required item }
          { If not found, continue to next trigger }
        end;

        { Display message if any }
        if Message <> '' then
        begin
          WriteLn;
          WriteLn(Message);
        end;

        { Execute action }
        case ActionType of
          actUnlockExit: begin
            { Unlock exit in target room }
            RoomIndex := FindRoom(TargetID);
            if RoomIndex >= 0 then
            begin
              case ActionValue of
                1: Rooms^[RoomIndex].NorthWay := ActionValue;
                2: Rooms^[RoomIndex].SouthWay := ActionValue;
                { etc... }
              end;
            end;
          end;

          actAddScore: begin
            Inc(Player.Score, ActionValue);
            WriteLn('Your score increased by ', ActionValue, ' points!');
          end;

          actSpawnItem: begin
            { Place item in target room }
            { Find item by TargetID and set Location to ActionValue }
          end;

          actTeleport: begin
            Player.CurrentRoom := ActionValue;
            WriteLn('You are suddenly transported elsewhere!');
            ShowRoom;
          end;

          actShowMessage: begin
            { Message already shown above }
          end;

          { Add other action implementations }
        end;

        { Mark as triggered }
        if OneTime then
          Triggered := True;
      end;
    end;
  end;
end;

{ Modified TakeItem to include trigger check }
procedure TakeItemWithTrigger(ItemName: String);
var
  i: Integer;
  Found: Boolean;
  ItemID: Integer;
begin
  Found := False;
  ItemName := UpCase(ItemName);

  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Active and
       (Items^[i].Location = Player.CurrentRoom) and
       (Pos(ItemName, UpCase(Items^[i].Name)) > 0) then
    begin
      Found := True;
      ItemID := Items^[i].ID;

      if Items^[i].Takeable then
      begin
        Items^[i].Location := -1; { Move to inventory }
        WriteLn('Taken: ', Items^[i].Name);
        Inc(Player.Score, 10);

        { Check for triggers }
        CheckTriggers(trgItemPickup, ItemID);
      end
      else
        WriteLn('You can''t take that.');
      Break;
    end;
  end;

  if not Found then
    WriteLn('You don''t see that here.');
end;

{ Modified room entry to check triggers }
procedure EnterRoomWithTrigger(RoomID: Integer);
begin
  Player.CurrentRoom := RoomID;
  Inc(Player.Moves);

  { Check room entry triggers }
  CheckTriggers(trgRoomEnter, RoomID);

  ShowRoom;
end;

{ USE command for item interactions }
procedure UseItem(ItemName: String);
var
  i: Integer;
  Found: Boolean;
  ItemID: Integer;
begin
  Found := False;
  ItemName := UpCase(ItemName);

  { Check inventory for item }
  for i := 0 to FileHeader.ItemRecordCount - 1 do
  begin
    if Items^[i].Active and
       (Items^[i].Location = -1) and
       (Pos(ItemName, UpCase(Items^[i].Name)) > 0) then
    begin
      Found := True;
      ItemID := Items^[i].ID;

      WriteLn('You use the ', Items^[i].Name, '.');

      { Check for use triggers }
      CheckTriggers(trgItemUse, ItemID);
      Break;
    end;
  end;

  if not Found then
    WriteLn('You don''t have that item.');
end;

{ Title screen }
procedure ShowTitle;
begin
  ClrScr;
  WriteLn('=====================================');
  WriteLn('     TEXT ADVENTURE ENGINE v1.0      ');
  WriteLn('=====================================');
  WriteLn;
  WriteLn('1. New Game');
  WriteLn('2. Load Game');
  WriteLn('3. Quit');
  WriteLn;
  Write('Select option: ');
end;

{ Main program }
var
  Choice: Char;

begin
  { Load configuration }
  LoadConfig;

  { Load database }
  if not LoadDatabase then
  begin
    WriteLn('Failed to load game data!');
    WriteLn('Press any key to exit...');
    ReadKey;
    Halt(1);
  end;

  { Check for required data }
  if FileHeader.RoomRecordCount = 0 then
  begin
    WriteLn('Error: No rooms in database!');
    WriteLn('Press any key to exit...');
    ReadKey;
    Halt(1);
  end;

  { Title screen }
  repeat
    ShowTitle;
    Choice := ReadKey;
    WriteLn(Choice);

    case Choice of
      '1': begin
        InitNewGame;
        WriteLn;
        WriteLn('Game loaded successfully!');
        WriteLn('Press any key to begin...');
        ReadKey;
        GameLoop;
        Break;
      end;
      '2': begin
        if LoadGame then
        begin
          WriteLn('Press any key to continue...');
          ReadKey;
          GameLoop;
        end
        else
        begin
          WriteLn('Press any key to return...');
          ReadKey;
        end;
      end;
      '3': Break;
    end;
  until Choice = '3';

  { Cleanup }
  Dispose(NPCs);
  Dispose(Items);
  Dispose(Rooms);

  ClrScr;
  WriteLn('Thanks for playing!');
  if Player.Score > 0 then
  begin
    WriteLn('Final score: ', Player.Score);
    WriteLn('Total moves: ', Player.Moves);
  end;
end.
