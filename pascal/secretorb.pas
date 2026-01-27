{ Secret Orb - A modular text-based adventure game }
{ Target: 720KB floppy, x86 compatible systems }
program SecretOrb;

{$MODE OBJFPC}

uses
  Crt, SysUtils,
  GameData, DataFile, Display, GameCore;

const
  VERSION = '0.1.0';
  DEFAULT_WORLD = 'world.dat';

var
  Game: TGame;
  WorldFile: string;

procedure ShowTitle;
begin
  ClearScreen;
  SetColor(LightCyan, Black);
  WriteCenter(8, '  ___                 _      ___       _    ');
  WriteCenter(9, ' / __| ___  __ _ _ __| |_   / _ \ _ _ | |__ ');
  WriteCenter(10, ' \__ \/ -_)/ _| ''_/ -_)  _| | (_) | ''_|| ''_ \');
  WriteCenter(11, ' |___/\___|\__|_| \___|\__|  \___/|_|  |_.__/');
  ResetColor;

  SetColor(Yellow, Black);
  WriteCenter(14, 'A Text Adventure');
  ResetColor;

  SetColor(DarkGray, Black);
  WriteCenter(16, 'Version ' + VERSION);
  ResetColor;

  WriteCenter(20, 'Press any key to begin...');
  WaitKey;
end;

procedure ShowError(const Msg: string);
begin
  ClearScreen;
  SetColor(LightRed, Black);
  WriteCenter(12, 'ERROR: ' + Msg);
  ResetColor;
  WriteCenter(14, 'Press any key to exit...');
  WaitKey;
end;

begin
  { Parse command line for world file }
  if ParamCount > 0 then
    WorldFile := ParamStr(1)
  else
    WorldFile := DEFAULT_WORLD;

  ShowTitle;

  { Load world data }
  if not LoadGame(Game, WorldFile) then
  begin
    ShowError('Could not load world file: ' + WorldFile);
    Halt(1);
  end;

  { Run the game }
  RunGame(Game);

  { Clean exit }
  ClearScreen;
end.
