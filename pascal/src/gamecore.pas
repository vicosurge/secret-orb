{ gamecore.pas - Core game engine for Secret Orb }
unit GameCore;

{$MODE OBJFPC}

interface

uses
  Crt, SysUtils, GameData, DataFile, Display;

type
  TCommandType = (
    cmdNone,
    cmdNorth, cmdSouth, cmdEast, cmdWest,
    cmdLook,
    cmdHelp,
    cmdQuit,
    cmdUnknown
  );

  TGameState = (gsPlaying, gsQuit);

  TGame = record
    World: TGameWorld;
    State: TGameState;
    LastMessage: string;
  end;

procedure InitGame(var G: TGame);
function LoadGame(var G: TGame; const FileName: string): Boolean;
procedure RunGame(var G: TGame);
function ParseCommand(const Input: string): TCommandType;
procedure ExecuteCommand(var G: TGame; Cmd: TCommandType);
procedure ShowRoom(var G: TGame);
procedure ShowHelp;

implementation

procedure InitGame(var G: TGame);
begin
  InitWorld(G.World);
  G.State := gsPlaying;
  G.LastMessage := '';
end;

function LoadGame(var G: TGame; const FileName: string): Boolean;
begin
  InitGame(G);
  Result := LoadWorld(FileName, G.World);
end;

function ParseCommand(const Input: string): TCommandType;
var
  Cmd: string;
begin
  Cmd := UpperCase(Trim(Input));

  if (Cmd = 'N') or (Cmd = 'NORTH') then
    Result := cmdNorth
  else if (Cmd = 'S') or (Cmd = 'SOUTH') then
    Result := cmdSouth
  else if (Cmd = 'E') or (Cmd = 'EAST') then
    Result := cmdEast
  else if (Cmd = 'W') or (Cmd = 'WEST') then
    Result := cmdWest
  else if (Cmd = 'L') or (Cmd = 'LOOK') then
    Result := cmdLook
  else if (Cmd = 'H') or (Cmd = 'HELP') or (Cmd = '?') then
    Result := cmdHelp
  else if (Cmd = 'Q') or (Cmd = 'QUIT') or (Cmd = 'EXIT') then
    Result := cmdQuit
  else if Cmd = '' then
    Result := cmdNone
  else
    Result := cmdUnknown;
end;

function Trim(const S: string): string;
var
  I, J: Integer;
begin
  I := 1;
  J := Length(S);
  while (I <= J) and (S[I] <= ' ') do Inc(I);
  while (J >= I) and (S[J] <= ' ') do Dec(J);
  Result := Copy(S, I, J - I + 1);
end;

procedure MovePlayer(var G: TGame; Dir: TDirection);
var
  Idx, TargetID, TargetIdx: Integer;
begin
  Idx := FindRoomByID(G.World, G.World.CurrentRoom);
  if Idx < 0 then
  begin
    G.LastMessage := 'Error: Current room not found!';
    Exit;
  end;

  TargetID := G.World.Rooms[Idx].Exits[Dir];
  if TargetID = DIR_NONE then
  begin
    G.LastMessage := 'You cannot go that way.';
    Exit;
  end;

  TargetIdx := FindRoomByID(G.World, TargetID);
  if TargetIdx < 0 then
  begin
    G.LastMessage := 'That path leads nowhere.';
    Exit;
  end;

  G.World.CurrentRoom := TargetID;
  G.LastMessage := 'You go ' + GetExitName(Dir) + '.';
end;

procedure ExecuteCommand(var G: TGame; Cmd: TCommandType);
begin
  case Cmd of
    cmdNorth: MovePlayer(G, dirNorth);
    cmdSouth: MovePlayer(G, dirSouth);
    cmdEast:  MovePlayer(G, dirEast);
    cmdWest:  MovePlayer(G, dirWest);
    cmdLook:  G.LastMessage := '';
    cmdHelp:  ShowHelp;
    cmdQuit:  G.State := gsQuit;
    cmdUnknown: G.LastMessage := 'I don''t understand that command.';
    cmdNone: ; { Do nothing }
  end;
end;

procedure ShowRoom(var G: TGame);
var
  Idx: Integer;
  Room: TRoom;
  Exits: string;
  D: TDirection;
begin
  ClearScreen;

  Idx := FindRoomByID(G.World, G.World.CurrentRoom);
  if Idx < 0 then
  begin
    WriteAt(1, 1, 'ERROR: Room not found!');
    Exit;
  end;

  Room := G.World.Rooms[Idx];

  { Room name }
  SetColor(Yellow, Black);
  WriteAt(1, 1, Room.Name);
  ResetColor;

  DrawHLine(1, Length(Room.Name), 2);

  { Description }
  WriteWrapped(1, 4, 78, Room.Desc);

  { Exits }
  Exits := 'Exits:';
  for D := Low(TDirection) to High(TDirection) do
    if Room.Exits[D] <> DIR_NONE then
      Exits := Exits + ' ' + GetExitName(D);

  if Exits = 'Exits:' then
    Exits := 'Exits: None';

  SetColor(Cyan, Black);
  WriteAt(1, 10, Exits);
  ResetColor;

  { Last message }
  if G.LastMessage <> '' then
  begin
    SetColor(LightGreen, Black);
    WriteAt(1, 12, G.LastMessage);
    ResetColor;
  end;

  { Command prompt }
  WriteAt(1, 14, '> ');
end;

procedure ShowHelp;
begin
  ClearScreen;
  SetColor(Yellow, Black);
  WriteCenter(2, '=== HELP ===');
  ResetColor;

  WriteAt(3, 5, 'Movement:');
  WriteAt(5, 6, 'N, NORTH  - Go north');
  WriteAt(5, 7, 'S, SOUTH  - Go south');
  WriteAt(5, 8, 'E, EAST   - Go east');
  WriteAt(5, 9, 'W, WEST   - Go west');

  WriteAt(3, 11, 'Other commands:');
  WriteAt(5, 12, 'L, LOOK   - Look around');
  WriteAt(5, 13, 'H, HELP   - Show this help');
  WriteAt(5, 14, 'Q, QUIT   - Quit the game');

  SetColor(Cyan, Black);
  WriteCenter(18, 'Press any key to continue...');
  ResetColor;
  WaitKey;
end;

procedure RunGame(var G: TGame);
var
  Input: string;
  Cmd: TCommandType;
begin
  InitDisplay;

  while G.State = gsPlaying do
  begin
    ShowRoom(G);
    Input := ReadLine(3, 14, 60);
    Cmd := ParseCommand(Input);
    ExecuteCommand(G, Cmd);
  end;

  ClearScreen;
  WriteCenter(12, 'Thanks for playing!');
  Delay(1500);
end;

end.
