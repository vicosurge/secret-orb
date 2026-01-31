program GameDataReader;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  // Game data file header (matches Python output)
  TGameHeader = packed record
    Magic: array[0..3] of Char;      // "GAME"
    Version: Word;                   // File format version
    RoomCount: LongWord;             // Number of rooms
    RoomDataOffset: LongWord;        // Offset to room data
    NPCDataOffset: LongWord;         // Future: NPC data offset (0 = not present)
    ObjectDataOffset: LongWord;      // Future: Object data offset (0 = not present)
  end;

  // Exit directions
  TExits = packed record
    North: LongInt;
    South: LongInt;
    East: LongInt;
    West: LongInt;
    Up: LongInt;
    Down: LongInt;
  end;

  // Dynamic string arrays
  TStringArray = array of String;

  // Room structure
  TRoom = record
    ID: LongWord;
    Name: String;
    Description: String;
    Exits: TExits;
    Lit: Boolean;
    Trigger: LongInt;
    NPC: LongInt;
    Visited: Boolean;
    Tags: TStringArray;
    Hazards: TStringArray;
    Fixtures: TStringArray;
    EventFlags: TStringArray;
  end;

  // Game data container
  TGameData = record
    Header: TGameHeader;
    Rooms: array of TRoom;
  end;

// Read a Pascal-style string (length byte + data)
function ReadPString(var F: File): String;
var
  Len: Byte;
  Buffer: array[0..255] of Char;
begin
  BlockRead(F, Len, 1);
  if Len > 0 then
  begin
    BlockRead(F, Buffer, Len);
    Buffer[Len] := #0;  // Null terminate
    Result := StrPas(@Buffer[0]);
  end
  else
    Result := '';
end;

// Read a dynamic string array (count byte + strings)
function ReadStringArray(var F: File): TStringArray;
var
  Count: Byte;
  i: Integer;
begin
  BlockRead(F, Count, 1);
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := ReadPString(F);
end;

// Read a single room from the binary file
function ReadRoom(var F: File): TRoom;
var
  BoolByte: Byte;
begin
  // Read room ID
  BlockRead(F, Result.ID, SizeOf(LongWord));
  
  // Read name and description
  Result.Name := ReadPString(F);
  Result.Description := ReadPString(F);
  
  // Read exits (6 x 4 bytes)
  BlockRead(F, Result.Exits, SizeOf(TExits));
  
  // Read lit flag
  BlockRead(F, BoolByte, 1);
  Result.Lit := BoolByte <> 0;
  
  // Read trigger and NPC IDs
  BlockRead(F, Result.Trigger, SizeOf(LongInt));
  BlockRead(F, Result.NPC, SizeOf(LongInt));
  
  // Read visited flag
  BlockRead(F, BoolByte, 1);
  Result.Visited := BoolByte <> 0;
  
  // Read dynamic arrays
  Result.Tags := ReadStringArray(F);
  Result.Hazards := ReadStringArray(F);
  Result.Fixtures := ReadStringArray(F);
  Result.EventFlags := ReadStringArray(F);
end;

// Load the entire game data file
function LoadGameData(const FileName: String; var GameData: TGameData): Boolean;
var
  F: File;
  i: Integer;
begin
  Result := False;
  
  // Initialize
  FillChar(GameData.Header, SizeOf(TGameHeader), 0);
  SetLength(GameData.Rooms, 0);
  
  // Check if file exists
  if not FileExists(FileName) then
  begin
    WriteLn('Error: File "', FileName, '" not found');
    Exit;
  end;
  
  // Open file
  Assign(F, FileName);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open file "', FileName, '"');
    Exit;
  end;
  
  try
    // Read header
    BlockRead(F, GameData.Header, SizeOf(TGameHeader));
    
    // Validate magic signature
    if (GameData.Header.Magic[0] <> 'G') or 
       (GameData.Header.Magic[1] <> 'A') or
       (GameData.Header.Magic[2] <> 'M') or
       (GameData.Header.Magic[3] <> 'E') then
    begin
      WriteLn('Error: Invalid file format (expected GAME magic signature)');
      Exit;
    end;
    
    // Check version
    if GameData.Header.Version <> 1 then
    begin
      WriteLn('Warning: File version ', GameData.Header.Version, ' may not be fully supported');
    end;
    
    WriteLn('File header:');
    WriteLn('  Magic: ', GameData.Header.Magic);
    WriteLn('  Version: ', GameData.Header.Version);
    WriteLn('  Room count: ', GameData.Header.RoomCount);
    WriteLn('  Room data offset: ', GameData.Header.RoomDataOffset);
    WriteLn;
    
    // Seek to room data
    Seek(F, GameData.Header.RoomDataOffset);
    
    // Read all rooms
    SetLength(GameData.Rooms, GameData.Header.RoomCount);
    for i := 0 to GameData.Header.RoomCount - 1 do
    begin
      GameData.Rooms[i] := ReadRoom(F);
      if (i + 1) mod 10 = 0 then
        WriteLn('Loaded ', i + 1, '/', GameData.Header.RoomCount, ' rooms...');
    end;
    
    WriteLn('Successfully loaded ', GameData.Header.RoomCount, ' rooms');
    Result := True;
    
  finally
    Close(F);
  end;
end;

// Find a room by ID (returns array index, -1 if not found)
function FindRoomByID(const GameData: TGameData; RoomID: LongWord): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(GameData.Rooms) - 1 do
  begin
    if GameData.Rooms[i].ID = RoomID then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Get room by ID (returns pointer to room, nil if not found)
function GetRoom(const GameData: TGameData; RoomID: LongWord): ^TRoom;
var
  Index: Integer;
begin
  Result := nil;
  Index := FindRoomByID(GameData, RoomID);
  if Index >= 0 then
    Result := @GameData.Rooms[Index];
end;

// Print room information (for debugging/testing)
procedure PrintRoom(const Room: TRoom);
var
  i: Integer;
begin
  WriteLn('=== Room ID: ', Room.ID, ' ===');
  WriteLn('Name: ', Room.Name);
  WriteLn('Description: ', Room.Description);
  WriteLn('Lit: ', Room.Lit);
  WriteLn('Visited: ', Room.Visited);
  WriteLn('Trigger ID: ', Room.Trigger);
  WriteLn('NPC ID: ', Room.NPC);
  
  Write('Exits: ');
  Write('N:', Room.Exits.North, ' ');
  Write('S:', Room.Exits.South, ' ');
  Write('E:', Room.Exits.East, ' ');
  Write('W:', Room.Exits.West, ' ');
  Write('U:', Room.Exits.Up, ' ');
  WriteLn('D:', Room.Exits.Down);
  
  if Length(Room.Tags) > 0 then
  begin
    Write('Tags: ');
    for i := 0 to Length(Room.Tags) - 1 do
    begin
      Write('"', Room.Tags[i], '"');
      if i < Length(Room.Tags) - 1 then Write(', ');
    end;
    WriteLn;
  end;
  
  if Length(Room.Hazards) > 0 then
  begin
    Write('Hazards: ');
    for i := 0 to Length(Room.Hazards) - 1 do
    begin
      Write('"', Room.Hazards[i], '"');
      if i < Length(Room.Hazards) - 1 then Write(', ');
    end;
    WriteLn;
  end;
  
  if Length(Room.Fixtures) > 0 then
  begin
    Write('Fixtures: ');
    for i := 0 to Length(Room.Fixtures) - 1 do
    begin
      Write('"', Room.Fixtures[i], '"');
      if i < Length(Room.Fixtures) - 1 then Write(', ');
    end;
    WriteLn;
  end;
  
  if Length(Room.EventFlags) > 0 then
  begin
    Write('Event Flags: ');
    for i := 0 to Length(Room.EventFlags) - 1 do
    begin
      Write('"', Room.EventFlags[i], '"');
      if i < Length(Room.EventFlags) - 1 then Write(', ');
    end;
    WriteLn;
  end;
  
  WriteLn;
end;

// Clean up memory (good practice for older systems)
procedure CleanupGameData(var GameData: TGameData);
var
  i: Integer;
begin
  for i := 0 to Length(GameData.Rooms) - 1 do
  begin
    SetLength(GameData.Rooms[i].Tags, 0);
    SetLength(GameData.Rooms[i].Hazards, 0);
    SetLength(GameData.Rooms[i].Fixtures, 0);
    SetLength(GameData.Rooms[i].EventFlags, 0);
  end;
  SetLength(GameData.Rooms, 0);
end;

// Simple room navigation example
procedure NavigateRooms(const GameData: TGameData);
var
  CurrentRoomID: LongInt;
  CurrentRoom: TRoom;
  Input: String;
  NewRoomID: LongInt;
  RoomIndex: Integer;
begin
  // Start at the default room (from JSON: "room_id": 4)
  CurrentRoomID := 4;
  
  WriteLn('=== Room Navigation Demo ===');
  WriteLn('Commands: n/s/e/w/u/d (directions), look, quit');
  WriteLn;
  
  repeat
    RoomIndex := FindRoomByID(GameData, CurrentRoomID);
    if RoomIndex < 0 then
    begin
      WriteLn('Error: Current room (ID ', CurrentRoomID, ') not found!');
      Break;
    end;
    
    CurrentRoom := GameData.Rooms[RoomIndex];
    
    // Show current room
    WriteLn('You are in: ', CurrentRoom.Name);
    WriteLn(CurrentRoom.Description);
    if not CurrentRoom.Lit then
      WriteLn('(This room is dark)');
    WriteLn;
    
    Write('> ');
    ReadLn(Input);
    Input := LowerCase(Trim(Input));
    
    if Input = 'quit' then
      Break
    else if Input = 'look' then
      PrintRoom(CurrentRoom)
    else if Input = 'n' then
    begin
      NewRoomID := CurrentRoom.Exits.North;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go north from here.');
    end
    else if Input = 's' then
    begin
      NewRoomID := CurrentRoom.Exits.South;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go south from here.');
    end
    else if Input = 'e' then
    begin
      NewRoomID := CurrentRoom.Exits.East;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go east from here.');
    end
    else if Input = 'w' then
    begin
      NewRoomID := CurrentRoom.Exits.West;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go west from here.');
    end
    else if Input = 'u' then
    begin
      NewRoomID := CurrentRoom.Exits.Up;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go up from here.');
    end
    else if Input = 'd' then
    begin
      NewRoomID := CurrentRoom.Exits.Down;
      if NewRoomID <> -1 then
        CurrentRoomID := NewRoomID
      else
        WriteLn('You cannot go down from here.');
    end
    else
      WriteLn('Unknown command. Try: n/s/e/w/u/d, look, quit');
      
    WriteLn;
  until False;
end;

// Main program
var
  GameData: TGameData;
  FileName: String;
  i: Integer;
  
begin
  WriteLn('Pascal Game Data Binary Reader');
  WriteLn('=============================');
  WriteLn;
  
  // Get filename from command line or use default
  if ParamCount > 0 then
    FileName := ParamStr(1)
  else
    FileName := 'structure.bin';
    
  WriteLn('Loading file: ', FileName);
  WriteLn;
  
  // Load the game data
  if not LoadGameData(FileName, GameData) then
  begin
    WriteLn('Failed to load game data. Exiting.');
    Exit;
  end;
  
  // Show summary
  WriteLn;
  WriteLn('=== Game Data Summary ===');
  WriteLn('Total rooms: ', Length(GameData.Rooms));
  for i := 0 to Length(GameData.Rooms) - 1 do
  begin
    Write('Room ', GameData.Rooms[i].ID, ': ', GameData.Rooms[i].Name);
    if Length(GameData.Rooms[i].Tags) > 0 then
      Write(' [', GameData.Rooms[i].Tags[0], ']');
    WriteLn;
  end;
  WriteLn;
  
  // Interactive demo
  WriteLn('Press Enter to start navigation demo, or Ctrl+C to exit...');
  ReadLn;
  
  NavigateRooms(GameData);
  
  // Clean up
  CleanupGameData(GameData);
  
  WriteLn('Goodbye!');
end.
