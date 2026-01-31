program QuickSave;
type
  TGameState = record
    playerX, playerY: integer;
    health: byte;
    level: byte;
    inventory: array[1..10] of string[20];
  end;

var
  f: file of TGameState;
  game: TGameState;
begin
  { Set up game state }
  game.playerX := 100;
  game.playerY := 200;
  game.health := 75;
  game.level := 3;
  game.inventory[1] := 'Sword';
  game.inventory[2] := 'Shield';
  
  { Save entire game state in ONE line! }
  Assign(f, 'savegame.dat');
  Rewrite(f);
  Write(f, game);
  Close(f);
  
  writeln('Game saved!');
end.
