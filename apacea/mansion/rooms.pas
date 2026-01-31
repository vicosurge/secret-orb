unit Rooms;

interface

uses
  Classes, SysUtils;

procedure AddRoom;
procedure ListAllRooms;
procedure ShowRoom;
procedure InitializeGameFile;

implementation

type
  { Room Header Structure }
  TGameHeader = record
    magic: array[1..4] of char;
    version: byte;
    totalRooms: word;
    startRoomID: shortint;
  end;

type
  TRoomFlag = (
    rfLit,
    rfDark,
    rfSafe,
    rfIndoor,
    rfMagic,
    rfSecret
  );
  TRoomFlags = set of TRoomFlag;
  { Room structure }
  TRoom = record
    name: string[12];
    description: string[80];
    { Directions }
    north, south, east, west, up, down: shortint;
    flags: TRoomFlags;
    tags: array[1..3] of string[10];
    decorations, npc, obj, triggers: array[1..3] of shortint;
  end;

var
  gameFile: file;
  header: TGameHeader;
  room: TRoom;

const
  MANSION_MAGIC = 'VMMM';
  ROOM_FILE = 'ROOMS.DAT';

{ Convert pipe to actual linebreaks }
function ConvertPipeBreaks(const input: string): string;
var
  converted, indent: string;
  i: integer;
begin
  indent := '   ';
  converted := indent;
  for i := 1 to Length(input) do
  begin
    if input[i] = '|' then
      converted := converted + Chr(10) + indent  { Convert | to line break }
    else
      converted := converted + input[i];
  end;
  ConvertPipeBreaks := converted;
end;

{ Initialize header }
procedure WriteHeader;
begin
  Assign(gameFile, ROOM_FILE);
  Rewrite(gameFile, 1);

  Move(MANSION_MAGIC, header.magic, 4);
  header.version := 1;
  header.totalRooms := 1;
  header.startRoomID := 2;

  BlockWrite(gameFile, header, SizeOf(TGameHeader));

  room.name := 'Shadow Zone';
  room.description := 'Shadow zone|I come to realize that I''m home now|But you can''t see inside me';
  room.north := -1;
  room.south := -1;
  room.east := -1;
  room.west := -1;
  room.up := -1;
  room.down := -1;
  Include(room.flags, rfLit);
  Include(room.flags, rfSafe);
  Include(room.flags, rfSecret);
  BlockWrite(gameFile, room, SizeOf(TRoom));
  Close(gameFile);
end;

{ Verify file exists and it is valid }
function FileExistsAndValid: boolean;
var
  f: file;
  tempHeader: TGameHeader;
begin
  FileExistsAndValid := false;

  Assign(f, ROOM_FILE);
  {$I-}
  Reset(f, 1);
  {$I+}

  if IOResult = 0 then
  begin
    BlockRead(f, tempHeader, SizeOf(TGameHeader));
    Close(f);

    { Copy to global header if valid }
    if tempHeader.magic = MANSION_MAGIC then
    begin
      header := tempHeader;
      FileExistsAndValid := true;
    end;
  end;
end;

{ Identify if files exist }
procedure InitializeGameFile;
begin
  if not FileExistsAndValid then
  begin
    WriteLn('Creating game file');
    WriteHeader;
    WriteLn('Finished creating ',ROOM_FILE);
  end
  else
  begin
    WriteLn('Found valid ',ROOM_FILE,' file');
    WriteLn('Loaded ', header.totalRooms, ' rooms.');
  end;
end;

{ Save room to file }
procedure SaveRoom(roomID: shortint; const room: TRoom);
var
  position: longint;
begin
  Assign(gameFile, ROOM_FILE);
  Reset(gameFile, 1);  { Important: 1-byte records }

  { Calculate position: header size + (roomID * room size) }
  position := SizeOf(TGameHeader) + (roomID * SizeOf(TRoom));
  Seek(gameFile, position);

  BlockWrite(gameFile, room, SizeOf(TRoom));
  Close(gameFile);
end;

{ Update header procedure }
procedure UpdateHeader;
begin
  Assign(gameFile, ROOM_FILE);
  Reset(gameFile, 1);
  Seek(gameFile, 0);  { Go to beginning }
  BlockWrite(gameFile, header, SizeOf(TGameHeader));
  Close(gameFile);
end;

{ Add a new room }
procedure AddRoom;
var
  input: string;
begin
  writeln('=== Add New Room ===');

  { Get input }
  write('Name: ');
  readln(room.name);

  write('Description: ');
  readln(room.description);

  write('North: ');
  readln(input);
  if input = '' then
    room.north := -1
  else
    room.north := StrToInt(input);

  write('South: ');
  readln(input);
  if input = '' then
    room.south := -1
  else
    room.south := StrToInt(input);

  write('East: ');
  readln(input);
  if input = '' then
    room.east := -1
  else
    room.east := StrToInt(input);

  write('West: ');
  readln(input);
  if input = '' then
    room.west := -1
  else
    room.west := StrToInt(input);

  write('Up: ');
  readln(input);
  if input = '' then
    room.up := -1
  else
    room.up := StrToInt(input);

  write('Down: ');
  readln(input);
  if input = '' then
    room.down := -1
  else
    room.down := StrToInt(input);

  write('Is the room lit? [Y/N]: ');
  readln(input);
  if UpCase(input) = 'Y' then Include(room.flags, rfLit);

  write('Is the room safe? [Y/N]: ');
  readln(input);
  if UpCase(input) = 'Y' then Include(room.flags, rfSafe);

  write('Save room? [Y/N] ');
  readln(input);
  if UpCase(input) = 'Y' then
     { Save the room at the new position }
     SaveRoom(header.totalRooms, room);

     { Increment room count }
     Inc(header.totalRooms);

     { Update header with new count }
     UpdateHeader;
     WriteLn('Room saved! Total rooms: ', header.totalRooms);
end;

{ Load rooms }
procedure LoadRoom(roomID: shortint; var room: TRoom);
var
  position: longint;
begin
  Assign(gameFile, ROOM_FILE);
  Reset(gameFile, 1);  { Important: 1-byte records }

  { Calculate position: header size + (roomID * room size) }
  position := SizeOf(TGameHeader) + ((roomID - 1) * SizeOf(TRoom));
  Seek(gameFile, position);

  BlockRead(gameFile, room, SizeOf(TRoom));
  Close(gameFile);
end;

{ List all rooms }
procedure ListAllRooms;
var
  room: TRoom;
  i: shortint;
begin
  if header.totalRooms = 0 then
  begin
    writeln('No rooms in database.');
    exit;
  end;

  writeln('All Rooms:');
  writeln('----------');

  for i := 1 to header.totalRooms do
  begin
    LoadRoom(i, room);
    writeln(i:2, '. ', room.name);
  end;
end;

{ Display room information }
procedure DisplayRoom(const room: TRoom);
var
  brokendescription: string;
begin
  writeln('Room: ', room.name);
  writeln('Description: ');
  brokendescription := ConvertPipeBreaks(room.description);
  write(brokendescription);
  writeln;

  { Show exits }
  writeln('Exits:');
  if room.north <> -1 then writeln('  North: Room ', room.north) else writeln('  North: None');
  if room.south <> -1 then writeln('  South: Room ', room.south) else writeln('  South: None');
  if room.east <> -1 then writeln('  East: Room ', room.east) else writeln('  East: None');
  if room.west <> -1 then writeln('  West: Room ', room.west) else writeln('  West: None');
  if room.up <> -1 then writeln('  Up: Room ', room.up) else writeln('  Up: None');
  if room.down <> -1 then writeln('  Down: Room ', room.down) else writeln('  Down: None');

  { Show flags }
  write('Properties: ');
  if rflit in room.flags then write('Lit ') else write('Dark ');
  if rfsafe in room.flags then write('Safe ') else write('Dangerous ');
  writeln;
end;

{ Look for specific room for display }
procedure ShowRoom;
var
  room: TRoom;
  roomID: shortint;
  input: string;
begin
  write('Enter room ID to view: ');
  readln(input);
  roomID := StrToInt(input);

  { Validate room ID }
  if (roomID < 1) or (roomID > header.totalRooms) then
  begin
    writeln('Invalid room ID. Valid range: 1-', header.totalRooms);
    exit;
  end;

  { Load and display }
  LoadRoom(roomID, room);
  DisplayRoom(room);
end;

end.

