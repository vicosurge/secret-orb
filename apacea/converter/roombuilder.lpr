program roombuilder;

type
  { Room structure }
  TRoom = record
    name: string[20];
    description: string[200];
    { Directions }
    north: shortint;
    south: shortint;
    east: shortint;
    west: shortint;
    up: shortint;
    down: shortint;
    tags: array[1..5] of string[10];
  end;

const
  databaseName = 'rooms.dat';
  indexName = 'rooms.idx';

var
  dataFile: file;
  indexFile: file;
  room: TRoom;
  choice: char;

{ Initialize new database files }
 procedure InitDatabase;
 var
   count: word;
 begin
   { Create/clear data file }
   Assign(dataFile, databaseName);
   Rewrite(dataFile, 1);
   Close(dataFile);

   { Create index file with count = 0 }
   Assign(indexFile, indexName);
   Rewrite(indexFile, 1);
   count := 0;
   BlockWrite(indexFile, count, 2);  { First 2 bytes = record count }
   Close(indexFile);

   writeln('Database initialized!');
 end;

{ Add a new room }
procedure AddRoom;
var
  count: word;
  offset: longint;
  tagInput: string;
  i: integer;
begin
  writeln('=== Add New Room ===');

  { Get input }
  write('Name: ');
  readln(room.name);

  write('Description: ');
  readln(room.description);

  write('North: ');
  readln(room.north);

  write('South: ');
  readln(room.south);

  write('East: ');
  readln(room.east);

  write('West: ');
  readln(room.west);

  write('Up: ');
  readln(room.up);

  write('Down: ');
  readln(room.down);

  { Get tags - up to 5 }
  writeln('Enter tags (max 5, max 10 chars each):');
  i := 1;
  repeat
    write('Tag ', i, ' (or press Enter to skip): ');
    readln(tagInput);

    if tagInput <> '' then
    begin
      room.tags[i] := tagInput;
      i := i + 1;
    end
    else
      break;  { Stop if empty input }

  until i > 5;

  { Fill remaining slots with empty strings }
  while i <= 5 do
  begin
    room.tags[i] := '';
    i := i + 1;
  end;


  { Open data file and append room at end }
  Assign(dataFile, databaseName);
  Reset(dataFile, 1);
  Seek(dataFile, FileSize(dataFile));  { Go to end }
  offset := FilePos(dataFile);         { Remember where we're writing }
  BlockWrite(dataFile, room, SizeOf(TRoom));
  Close(dataFile);

  { Update index file }
  Assign(indexFile, indexName);
  Reset(indexFile, 1);

  { Read current count }
  BlockRead(indexFile, count, 2);

  { Go to end and add new offset }
  Seek(indexFile, FileSize(indexFile));
  BlockWrite(indexFile, offset, 4);  { Write the offset (4 bytes) }

  { Update count at beginning }
  count := count + 1;
  Seek(indexFile, 0);
  BlockWrite(indexFile, count, 2);

  Close(indexFile);
  writeln('Room #', count, ' added!');
end;

{ List all rooms using the index }
procedure ListRooms;
var
  count: word;
  tagcheck: integer;
  offset: longint;
  i, j: integer;
begin
  writeln('=== Room List [The Mansion] ===');

  { Read index }
  Assign(indexFile, indexName);
  Reset(indexFile, 1);
  BlockRead(indexFile, count, 2);

  if count = 0 then
  begin
    writeln('No rooms in database!');
    Close(indexFile);
    exit;
  end;

  { Open data file }
  Assign(dataFile, databaseName);
  Reset(dataFile, 1);

  { Read each room using index }
  for i := 1 to count do
  begin
    { Get offset from index }
    BlockRead(indexFile, offset, 4);

    { Jump to that position in data file }
    Seek(dataFile, offset);
    BlockRead(dataFile, room, SizeOf(TRoom));

    { Display room }
    writeln;
    writeln('Room #', i);
    writeln('  Name: ', room.name);
    writeln('  Description: ', room.description);
    write('  Tags: ');
    tagcheck := 0;
    for j := 1 to 5 do
    begin
      if room.tags[j] <> '' then
      begin
        if tagcheck > 0 then write(', ');
        write(room.tags[j]);
        tagcheck := tagcheck + 1;
      end;
    end;
    if tagcheck = 0 then write('(none)');
    writeln;
  end;

  Close(indexFile);
  Close(dataFile);

  writeln;
  writeln('Total amount of rooms: ', count);
end;

{ Show raw file structure for learning }
procedure ShowStructure;
var
  count: word;
  offset: longint;
  i: integer;
begin
  writeln('=== Database Structure ===');

  { Show index structure }
  Assign(indexFile, indexName);
  Reset(indexFile, 1);
  BlockRead(indexFile, count, 2);

  writeln('INDEX FILE:');
  writeln('  Record count: ', count);
  writeln('  Offsets:');

  for i := 1 to count do
  begin
    BlockRead(indexFile, offset, 4);
    writeln('    Room #', i, ' starts at byte: ', offset);
  end;
  Close(indexFile);

  { Show data file size }
  Assign(dataFile, databaseName);
  Reset(dataFile, 1);
  writeln;
  writeln('DATA FILE:');
  writeln('  Size: ', FileSize(dataFile), ' bytes');
  writeln('  Record size: ', SizeOf(TRoom), ' bytes each');
  Close(dataFile);
end;


{ Main program }
begin
  writeln('Room Structure Builder [Database Manager]');
  writeln('====================');

  { Check if database exists }
  {$I-}
  Assign(indexFile, indexName);
  Reset(indexFile, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    writeln('No database found. Creating new one...');
    InitDatabase;
  end
  else
    Close(indexFile);

  { Menu loop }
  repeat
    writeln;
    writeln('1. Add room');
    writeln('2. List all rooms');
    writeln('3. Show file structure');
    writeln('4. Quit');
    write('Choice: ');
    readln(choice);
    writeln;

    case choice of
      '1': AddRoom;
      '2': ListRooms;
      '3': ShowStructure;
      '4': writeln('Goodbye!');
    else
      writeln('Invalid choice!');
    end;
  until choice = '4';
end.

