{ pairtest.pas - Exercise PairExits without going through an editor UI }
program PairTest;

{$MODE OBJFPC}

uses
  SysUtils, GameData, DataFile, WorldVal;

var
  W: TGameWorld;
  Failures: Integer;

procedure Check(const What: string; Got, Want: Integer);
begin
  if Got = Want then
    WriteLn('  ok    ', What, ' = ', Got)
  else
  begin
    WriteLn('  FAIL  ', What, ' = ', Got, ', expected ', Want);
    Inc(Failures);
  end;
end;

procedure Reset3Rooms;
var
  I: Integer;
begin
  InitWorld(W);
  for I := 1 to 3 do
  begin
    W.Rooms[I].ID := I;
    W.Rooms[I].Name := 'Room ' + IntToStr(I);
    W.Rooms[I].Active := True;
  end;
  W.RoomCount := 3;
  W.CurrentRoom := 1;
end;

begin
  Failures := 0;

  WriteLn('a one-way exit gets a return exit');
  Reset3Rooms;
  W.Rooms[1].Exits[dirNorth] := 2;
  Check('pairs found', PairExits(W, 1, False), 1);
  Check('pairs applied', PairExits(W, 1, True), 1);
  Check('room 2 south', W.Rooms[2].Exits[dirSouth], 1);
  Check('nothing left to pair', PairExits(W, 1, False), 0);

  WriteLn('an exit that already leads back is left alone');
  Reset3Rooms;
  W.Rooms[1].Exits[dirNorth] := 2;
  W.Rooms[2].Exits[dirSouth] := 1;
  Check('pairs found', PairExits(W, 1, False), 0);

  WriteLn('a deliberate one-way link is not overwritten');
  Reset3Rooms;
  W.Rooms[1].Exits[dirNorth] := 2;
  W.Rooms[2].Exits[dirSouth] := 3;   { south already goes elsewhere }
  Check('pairs found', PairExits(W, 1, False), 0);
  PairExits(W, 1, True);
  Check('room 2 south untouched', W.Rooms[2].Exits[dirSouth], 3);

  WriteLn('an exit to a room that does not exist is skipped');
  Reset3Rooms;
  W.Rooms[1].Exits[dirEast] := 99;
  Check('pairs found', PairExits(W, 1, False), 0);

  WriteLn('a self-loop is skipped');
  Reset3Rooms;
  W.Rooms[1].Exits[dirUp] := 1;
  Check('pairs found', PairExits(W, 1, False), 0);

  WriteLn('every direction pairs with its opposite');
  Reset3Rooms;
  W.Rooms[1].Exits[dirNorth] := 2;
  W.Rooms[1].Exits[dirEast] := 3;
  W.Rooms[1].Exits[dirDown] := 2;
  Check('pairs found', PairExits(W, 1, False), 3);
  PairExits(W, 1, True);
  Check('room 2 south', W.Rooms[2].Exits[dirSouth], 1);
  Check('room 3 west', W.Rooms[3].Exits[dirWest], 1);
  Check('room 2 up', W.Rooms[2].Exits[dirUp], 1);

  WriteLn;
  if Failures = 0 then
    WriteLn('PairExits: all checks passed')
  else
  begin
    WriteLn('PairExits: ', Failures, ' failure(s)');
    Halt(1);
  end;
end.
