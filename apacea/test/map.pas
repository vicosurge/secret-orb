program AdventureMap;

const
  MAX_ROOMS = 100;
  NO_EXIT = -1;

type
  TRoom = record
    id: integer;
    name: string[50];
    description: string[200];
    { Exit connections - store room IDs }
    north: shortint;
    south: shortint;
    east: shortint;
    west: shortint;
    up: shortint;
    down: shortint;
    lit: boolean;
    tags: array[1..5] of string[10];
  end;
  
  TWorld = record
    rooms: array[0..MAX_ROOMS-1] of TRoom;
    roomCount: integer;
    currentRoom: integer;  { Where player is }
  end;

var
  world: TWorld;
  command: string;

{ Initialize world }
procedure InitWorld;
begin
  world.roomCount := 0;
  world.currentRoom := 0;
end;

{ Create a room and return its ID }
function CreateRoom(name, desc: string): integer;
var
  id: integer;
  i: integer;
begin
  id := world.roomCount;
  
  with world.rooms[id] do
  begin
    id := id;
    name := name;
    description := desc;
    north := NO_EXIT;
    south := NO_EXIT;
    east := NO_EXIT;
    west := NO_EXIT;
    up := NO_EXIT;
    down := NO_EXIT;
    lit := true;
    
    for i := 1 to 5 do
      tags[i] := '';
  end;
  
  world.roomCount := world.roomCount + 1;
  CreateRoom := id;
end;

{ Connect two rooms }
procedure ConnectRooms(room1, dir1: integer; room2, dir2: integer);
begin
  { dir: 0=north, 1=south, 2=east, 3=west, 4=up, 5=down }
  
  { Set room1's exit }
  case dir1 of
    0: world.rooms[room1].north := room2;
    1: world.rooms[room1].south := room2;
    2: world.rooms[room1].east := room2;
    3: world.rooms[room1].west := room2;
    4: world.rooms[room1].up := room2;
    5: world.rooms[room1].down := room2;
  end;
  
  { Set room2's opposite exit }
  case dir2 of
    0: world.rooms[room2].north := room1;
    1: world.rooms[room2].south := room1;
    2: world.rooms[room2].east := room1;
    3: world.rooms[room2].west := room1;
    4: world.rooms[room2].up := room1;
    5: world.rooms[room2].down := room1;
  end;
end;

{ Build a sample world }
procedure BuildSampleWorld;
var
  entrance, hall, bookhall, basement, tower: integer;
begin
  { Create rooms }
  entrance := CreateRoom('Entrance Hall', 
    'A grand entrance with a chandelier hanging from the ceiling.');
  world.rooms[entrance].tags[1] := 'start';
  
  hall := CreateRoom('Long Hallway', 
    'A narrow hallway with portraits on the walls.');
  world.rooms[hall].tags[1] := 'creepy';
  
  bookhall := CreateRoom('Dusty Library', 
    'Shelves of ancient books reach the ceiling.');
  world.rooms[bookhall].tags[1] := 'books';
  world.rooms[bookhall].tags[2] := 'quiet';
  
  basement := CreateRoom('Dark Basement', 
    'It''s very dark here. You can barely see.');
  world.rooms[basement].lit := false;
  world.rooms[basement].tags[1] := 'dark';
  world.rooms[basement].tags[2] := 'scary';
  
  tower := CreateRoom('Tower Room', 
    'A circular room with windows on all sides.');
  world.rooms[tower].tags[1] := 'view';
  
  { Connect rooms: ConnectRooms(from, direction, to, opposite_direction) }
  ConnectRooms(entrance, 0, hall, 1);      { entrance north <-> hall south }
  ConnectRooms(hall, 2, bookhall, 3);       { hall east <-> library west }
  ConnectRooms(entrance, 5, basement, 4);  { entrance down <-> basement up }
  ConnectRooms(hall, 4, tower, 5);         { hall up <-> tower down }
  
  world.currentRoom := entrance;
end;

{ Display current room }
procedure ShowRoom;
var
  room: TRoom;
  i: integer;
begin
  room := world.rooms[world.currentRoom];
  
  writeln;
  writeln('=== ', room.name, ' ===');
  writeln(room.description);
  
  if not room.lit then
    writeln('** It''s very dark here! **');
  
  { Show exits }
  write('Exits: ');
  if room.north <> NO_EXIT then write('[North] ');
  if room.south <> NO_EXIT then write('[South] ');
  if room.east <> NO_EXIT then write('[East] ');
  if room.west <> NO_EXIT then write('[West] ');
  if room.up <> NO_EXIT then write('[Up] ');
  if room.down <> NO_EXIT then write('[Down] ');
  writeln;
  
  { Show tags if any }
  write('(Tags: ');
  for i := 1 to 5 do
    if room.tags[i] <> '' then
      write(room.tags[i], ' ');
  writeln(')');
end;

{ Move player }
function Move(direction: string): boolean;
var
  newRoom: shortint;
  room: TRoom;
begin
  room := world.rooms[world.currentRoom];
  newRoom := NO_EXIT;
  
  if (direction = 'n') or (direction = 'north') then
    newRoom := room.north
  else if (direction = 's') or (direction = 'south') then
    newRoom := room.south
  else if (direction = 'e') or (direction = 'east') then
    newRoom := room.east
  else if (direction = 'w') or (direction = 'west') then
    newRoom := room.west
  else if (direction = 'u') or (direction = 'up') then
    newRoom := room.up
  else if (direction = 'd') or (direction = 'down') then
    newRoom := room.down;
  
  if newRoom <> NO_EXIT then
  begin
    world.currentRoom := newRoom;
    Move := true;
  end
  else
  begin
    writeln('You can''t go that way!');
    Move := false;
  end;
end;

{ Save world to file }
procedure SaveWorld;
var
  f: file;
begin
  Assign(f, 'world.dat');
  Rewrite(f, 1);
  BlockWrite(f, world, SizeOf(TWorld));
  Close(f);
  writeln('World saved!');
end;

{ Load world from file }
procedure LoadWorld;
var
  f: file;
begin
  {$I-}
  Assign(f, 'world.dat');
  Reset(f, 1);
  {$I+}
  if IOResult = 0 then
  begin
    BlockRead(f, world, SizeOf(TWorld));
    Close(f);
    writeln('World loaded!');
  end
  else
    writeln('No saved world found.');
end;

{ Show map for debugging }
procedure ShowMap;
var
  i: integer;
  room: TRoom;
begin
  writeln('=== World Map ===');
  for i := 0 to world.roomCount - 1 do
  begin
    room := world.rooms[i];
    writeln('Room ', i, ': ', room.name);
    if room.north <> NO_EXIT then 
      writeln('  North -> ', world.rooms[room.north].name);
    if room.south <> NO_EXIT then 
      writeln('  South -> ', world.rooms[room.south].name);
    if room.east <> NO_EXIT then 
      writeln('  East -> ', world.rooms[room.east].name);
    if room.west <> NO_EXIT then 
      writeln('  West -> ', world.rooms[room.west].name);
    if room.up <> NO_EXIT then 
      writeln('  Up -> ', world.rooms[room.up].name);
    if room.down <> NO_EXIT then 
      writeln('  Down -> ', world.rooms[room.down].name);
  end;
end;

{ Main game loop }
begin
  writeln('Simple Adventure Engine');
  writeln('=======================');
  writeln('Commands: n/s/e/w/u/d to move, "map" to see connections, "save"/"load", "quit"');
  
  InitWorld;
  BuildSampleWorld;
  
  ShowRoom;
  
  repeat
    writeln;
    write('> ');
    readln(command);
    command := LowerCase(command);
    
    if (command = 'n') or (command = 'north') or
       (command = 's') or (command = 'south') or
       (command = 'e') or (command = 'east') or
       (command = 'w') or (command = 'west') or
       (command = 'u') or (command = 'up') or
       (command = 'd') or (command = 'down') then
    begin
      if Move(command) then
        ShowRoom;
    end
    else if command = 'map' then
      ShowMap
    else if command = 'save' then
      SaveWorld
    else if command = 'load' then
    begin
      LoadWorld;
      ShowRoom;
    end
    else if (command = 'quit') or (command = 'exit') then
      writeln('Goodbye!')
    else if command = '' then
      { Do nothing on empty input }
    else
      writeln('I don''t understand "', command, '"');
      
  until (command = 'quit') or (command = 'exit');
end.
