unit gameunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, roomunit;

type
  Game = class
    private
    map: array[0..3] of Room;
    pos: integer;
    public
    procedure Look;
    procedure Init;
    procedure N;
    procedure S;
    procedure E;
    procedure W;
    procedure U;
    procedure D;
    constructor Create;
  end;

implementation

procedure Game.Look;
begin
  Writeln('You are in the ' +map[pos].GetDescription+'.');
end;

procedure Game.Init;
begin
  pos := 0;
  map[0] := Room.Create('Troll Room', 'a smelly cave', -1, 2, -1, 1, -1, -1);
  map[1] := Room.Create('Forest', 'a dense woodland, like a deadly one', -1, -1, 0, -1, -1, -1);
  map[2] := Room.Create('Dome', 'a huge crystal dome', 0, -1, -1, 3, -1, -1);
  map[3] := Room.Create('Tomb', 'a vast echoing tomb', -1, -1, 2, -1, -1, -1);
end;

procedure Game.N;
begin
  if map[pos].N = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].N;
     Look;
   end;
end;

procedure Game.S;
begin
  if map[pos].S = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].S;
     Look;
   end;
end;

procedure Game.E;
begin
  if map[pos].E = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].E;
     Look;
   end;
end;

procedure Game.W;
begin
  if map[pos].W = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].W;
     Look;
   end;
end;

procedure Game.U;
begin
  if map[pos].U = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].U;
     Look;
   end;
end;

procedure Game.D;
begin
  if map[pos].D = -1 then
     Writeln('That is not an exit')
   else
   begin
     pos := map[pos].D;
     Look;
   end;
end;

procedure Game.MainLoop;
var
  input: char;
begin
  repeat
    Write('> ');
    Readln(input);
    case input of
      'n': N;
      's': S;
      'e': E;
      'w': W;
      'u': U;
      'd': D;
      'l': Look;
    end;
  until input = 'q';
  Writeln('Press key to end...');
  Readln;
end;

constructor Game.Create;
begin
  inherited Create;
  init;
end;

end.