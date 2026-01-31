program adv;

uses roomunit, gameunit;

procedure RunGame;
var
  g: Game;
begin
  g := Game.Create;
  g.MainLoop;
end;

begin
  RunGame;
end.