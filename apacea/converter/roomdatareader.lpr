program roomdatareader;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  // Game data file header
  TGameHeader = packed record
    Magic: array[0..3] of Char;      // "GAME"
    Version: Word;                   // File format version
    RoomCount: LongInt;              // Number of rooms
    RoomDataOffset: LongInt;         // Offset to room data
    NPCDataOffset: LongInt;          // Future: NPC data offset (0 = not present)
    ObjectDataOffset: LongInt;       // Future: Object data offset (0 = not present)
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
    ID: LongInt;
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

// Read a dynamic string array
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
  // Read basic room data
  BlockRead(F, Result.ID, SizeOf(LongInt));
  Result.Name := ReadPString(F);
  Result.Description := ReadPString(F);

  // Read exits
  BlockRead(F, Result.Exits, SizeOf(TExits));

  // Read boolean fields (stored as bytes)
  BlockRead(F, BoolByte, 1);
  Result.Lit := BoolByte <> 0;

  // Read integer fields
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
function LoadGameData(const FileName: String): TGameData;
var
  F: File;
  i: Integer;
begin
  // Initialize result
  FillChar(Result.Header, SizeOf(TGameHeader), 0);
  SetLength(Result.Rooms, 0);

  // Open file
  Assign(F, FileName);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open file ', FileName);
    Exit;
  end;

  try
    // Read header
    BlockRead(F, Result.Header, SizeOf(TGameHeader));

    // Validate magic signature
    if (Result.Header.Magic[0] <> 'G') or
       (Result.Header.Magic[1] <> 'A') or
       (Result.Header.Magic[2] <> 'M') or
       (Result.Header.Magic[3] <> 'E') then
    begin
      WriteLn('Error: Invalid file format (bad magic signature)');
      Exit;
    end;

    // Check version
    if Result.Header.Version <> 1 then
    begin
      WriteLn('Warning: File version ', Result.Header.Version, ' may not be supported');
    end;

    // Seek to room data
    Seek(F, Result.Header.RoomDataOffset);

    // Read all rooms
    SetLength(Result.Rooms, Result.Header.RoomCount);
    for i := 0 to Result.Header.RoomCount - 1 do
    begin
      Result.Rooms[i] := ReadRoom(F);
    end;

    WriteLn('Successfully loaded ', Result.Header.RoomCount, ' rooms');

  finally
    Close(F);
  end;
end;

// Find a room by ID
function FindRoom(const GameData: TGameData; RoomID: LongInt): Integer;
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

// Print room information (for testing)
procedure PrintRoom(const Room: TRoom);
var
  i: Integer;
begin
  WriteLn('Room ID: ', Room.ID);
  WriteLn('Name: ', Room.Name);
  WriteLn('Description: ', Room.Description);
  WriteLn('Lit: ', Room.Lit);
  WriteLn('Visited: ', Room.Visited);
  WriteLn('Trigger: ', Room.Trigger);
  WriteLn('NPC: ', Room.NPC);

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
      Write(Room.Tags[i]);
      if i < Length(Room.Tags) - 1 then Write(', ');
    end;
    WriteLn;
  end;

  if Length(Room.Hazards) > 0 then
  begin
    Write('Hazards: ');
    for i := 0 to Length(Room.Hazards) - 1 do
    begin
      Write(Room.Hazards[i]);
      if i < Length(Room.Hazards) - 1 then Write(', ');
    end;
    WriteLn;
  end;

  WriteLn('---');
end;

// Main program
var
  GameData: TGameData;
  i, RoomIndex: Integer;

begin
  WriteLn('Room Data Binary Reader');
  WriteLn('======================');

  // Load the game data
  GameData := LoadGameData('gamedata.bin');

  if Length(GameData.Rooms) = 0 then
  begin
    WriteLn('No rooms loaded. Exiting.');
    Exit;
  end;

  // Print all rooms (for testing)
  WriteLn;
  WriteLn('All Rooms:');
  WriteLn('----------');
  for i := 0 to Length(GameData.Rooms) - 1 do
    PrintRoom(GameData.Rooms[i]);

  // Example: Find a specific room
  WriteLn;
  WriteLn('Finding room with ID 4:');
  RoomIndex := FindRoom(GameData, 4);
  if RoomIndex >= 0 then
  begin
    WriteLn('Found room at index: ', RoomIndex);
    PrintRoom(GameData.Rooms[RoomIndex]);
  end
  else
    WriteLn('Room with ID 4 not found');

  // Clean up memory (automatic in modern Free Pascal, but good practice)
  for i := 0 to Length(GameData.Rooms) - 1 do
  begin
    SetLength(GameData.Rooms[i].Tags, 0);
    SetLength(GameData.Rooms[i].Hazards, 0);
    SetLength(GameData.Rooms[i].Fixtures, 0);
    SetLength(GameData.Rooms[i].EventFlags, 0);
  end;
  SetLength(GameData.Rooms, 0);

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
