program roombuilder;

type
  { Room structure }
  TRoom = record
    name: string[50];
    description: string[200];
    { Directions }
    north: byte;
    south: byte;
    east: byte;
    west: byte;
    up: byte;
    down: byte;
    lit: boolean;
  end;

const
  databaseName = 'rooms.dat';
  indexName = 'rooms.idx';

var
  dataFile: file;
  indexFile: file;
  room: TRoom;
  chooice: char;

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

{ Add a new movie }
procedure AddMovie;
var
  count: word;
  offset: longint;
  litChar: char;
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

  write('Lit? (y/n): ');
  readln(litChar);
  room.lit := UpCase(litChar) = 'Y';

  { Open data file and append movie at end }
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
    writeln('1. Add movie');
    writeln('2. List all movies');
    writeln('3. Show file structure');
    writeln('4. Quit');
    write('Choice: ');
    readln(choice);
    writeln;

    case choice of
      '1': AddMovie;
      '2': ListMovies;
      '3': ShowStructure;
      '4': writeln('Goodbye!');
    else
      writeln('Invalid choice!');
    end;
  until choice = '4';
end.

