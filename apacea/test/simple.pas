program SimpleDatabase;
const
  MAGIC_NUMBER = $4442;  { Changed name to avoid conflict }
  FILE_VERSION = 1;      { Changed name to avoid conflict }
  MAX_ARRAY_ITEMS = 5;

type
  TRecord = record
    name: string;
    description: string;
    location: string;
    itemCount: byte;
    items: array[0..MAX_ARRAY_ITEMS-1] of string;
  end;

var
  f: file;
  rec: TRecord;
  
{ Write a string with length byte }
procedure WriteString(var f: file; s: string);
var
  len: byte;
begin
  len := Length(s);
  BlockWrite(f, len, 1);        { Write length byte }
  if len > 0 then
    BlockWrite(f, s[1], len);   { Write actual string }
end;

{ Read a string with length byte }
function ReadString(var f: file): string;
var
  len: byte;
  s: string;  { Declare a local variable instead of using Result }
begin
  BlockRead(f, len, 1);
  if len > 0 then
  begin
    SetLength(s, len);
    BlockRead(f, s[1], len);
  end
  else
    s := '';
  
  ReadString := s;  { This is how older Pascal returns values }
end;

{ Main program }
var
  magic: word;
  version: word;
  recordCount: word;
  
begin
  { Create a test record }
  rec.name := 'Test Item';
  rec.description := 'A description';
  rec.location := 'Warehouse A';
  rec.itemCount := 2;
  rec.items[0] := 'Tag1';
  rec.items[1] := 'Tag2';
  
  { Write file }
  Assign(f, 'database.dat');
  Rewrite(f, 1);
  
  { Write header }
  magic := MAGIC_NUMBER;      { Now using the const value }
  version := FILE_VERSION;     { Now using the const value }
  recordCount := 1;
  
  BlockWrite(f, magic, 2);
  BlockWrite(f, version, 2);
  BlockWrite(f, recordCount, 2);
  
  { Write the record - simplified }
  WriteString(f, rec.name);
  WriteString(f, rec.description);
  WriteString(f, rec.location);
  
  Close(f);
  writeln('File written!');
  
  { Now read it back }
  Assign(f, 'database.dat');
  Reset(f, 1);
  
  { Read header }
  BlockRead(f, magic, 2);
  BlockRead(f, version, 2);
  BlockRead(f, recordCount, 2);
  
  writeln('Magic: ', magic);
  writeln('Version: ', version);
  writeln('Records: ', recordCount);
  
  { Read the record back }
  rec.name := ReadString(f);
  rec.description := ReadString(f);
  rec.location := ReadString(f);
  
  writeln('Name: ', rec.name);
  writeln('Description: ', rec.description);
  writeln('Location: ', rec.location);
  
  Close(f);
end.
