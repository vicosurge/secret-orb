{$MODE TP}
{$PACKRECORDS 1}
program MiniAdv;

const
  MAX_ROOMS = 50;
  
type
  TRoom = packed record
    n, s, e, w: shortint;  { Exits, -1 = none }
    desc: byte;            { Index to description file }
  end;

var
  rooms: array[0..MAX_ROOMS] of TRoom;
  pos: byte;
  cmd: char;
  f: file of TRoom;

procedure ShowRoom;
var
  tf: text;
  s: string;
  i: byte;
begin
  { Load description from file }
  Assign(tf, 'desc.txt');
  Reset(tf);
  for i := 0 to rooms[pos].desc do
    readln(tf, s);
  Close(tf);
  writeln(s);
  
  { Show exits }
  write('Exits: ');
  if rooms[pos].n >= 0 then write('N ');
  if rooms[pos].s >= 0 then write('S ');
  if rooms[pos].e >= 0 then write('E ');
  if rooms[pos].w >= 0 then write('W ');
  writeln;
end;

procedure Move(dir: char);
var
  newPos: shortint;
begin
  newPos := -1;
  case dir of
    'N': newPos := rooms[pos].n;
    'S': newPos := rooms[pos].s;
    'E': newPos := rooms[pos].e;
    'W': newPos := rooms[pos].w;
  end;
  
  if newPos >= 0 then
    pos := newPos
  else
    writeln('No exit!');
end;

procedure LoadWorld;
begin
  Assign(f, 'world.dat');
  Reset(f);
  Read(f, rooms);
  Close(f);
end;

begin
  LoadWorld;
  pos := 0;
  
  repeat
    ShowRoom;
    write('> ');
    readln(cmd);
    cmd := UpCase(cmd);
    
    if cmd in ['N','S','E','W'] then
      Move(cmd);
  until cmd = 'Q';
end.
