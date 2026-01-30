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
    cmdUnknown,
    cmdExamine,
    cmdTake,
    cmdDrop,
    cmdUse,
    cmdOpen,
    cmdRead,
    cmdTalk,
    cmdInventory
  );

  TGameState = (gsPlaying, gsQuit);

  TGame = record
    World: TGameWorld;
    State: TGameState;
    LastMessage: string;
    LastNoun: string[40];
  end;

procedure InitGame(var G: TGame);
function LoadGame(var G: TGame; const FileName: string): Boolean;
procedure RunGame(var G: TGame);
function ParseCommand(const Input: string; var Noun: string): TCommandType;
procedure ExecuteCommand(var G: TGame; Cmd: TCommandType);
procedure ShowRoom(var G: TGame);
procedure ShowHelp;
procedure HandleExamine(var G: TGame; const Noun: string);
procedure HandleTake(var G: TGame; const Noun: string);
procedure HandleDrop(var G: TGame; const Noun: string);
procedure HandleUse(var G: TGame; const Noun: string);
procedure HandleOpen(var G: TGame; const Noun: string);
procedure HandleRead(var G: TGame; const Noun: string);
procedure HandleTalk(var G: TGame; const Noun: string);
procedure HandleInventory(var G: TGame);

implementation

procedure InitGame(var G: TGame);
begin
  InitWorld(G.World);
  G.State := gsPlaying;
  G.LastMessage := '';
  G.LastNoun := '';
end;

function LoadGame(var G: TGame; const FileName: string): Boolean;
begin
  InitGame(G);
  Result := LoadWorld(FileName, G.World);
end;

function ParseCommand(const Input: string; var Noun: string): TCommandType;
var
  Cmd, FullCmd: string;
  SpacePos: Integer;
begin
  FullCmd := UpperCase(Trim(Input));
  Noun := '';

  { Find first word }
  SpacePos := Pos(' ', FullCmd);
  if SpacePos > 0 then
  begin
    Cmd := Copy(FullCmd, 1, SpacePos - 1);
    Noun := Trim(Copy(FullCmd, SpacePos + 1, Length(FullCmd)));
    { Handle "LOOK AT" as EXAMINE }
    if (Cmd = 'LOOK') and (Pos('AT ', Noun) = 1) then
    begin
      Noun := Trim(Copy(Noun, 4, Length(Noun)));
      Result := cmdExamine;
      Exit;
    end;
  end
  else
    Cmd := FullCmd;

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
  else if (Cmd = 'X') or (Cmd = 'EXAMINE') or (Cmd = 'EX') then
    Result := cmdExamine
  else if (Cmd = 'TAKE') or (Cmd = 'GET') or (Cmd = 'GRAB') then
    Result := cmdTake
  else if (Cmd = 'DROP') or (Cmd = 'PUT') then
    Result := cmdDrop
  else if (Cmd = 'USE') then
    Result := cmdUse
  else if (Cmd = 'OPEN') then
    Result := cmdOpen
  else if (Cmd = 'READ') then
    Result := cmdRead
  else if (Cmd = 'TALK') or (Cmd = 'SAY') or (Cmd = 'SPEAK') then
    Result := cmdTalk
  else if (Cmd = 'I') or (Cmd = 'INV') or (Cmd = 'INVENTORY') then
    Result := cmdInventory
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
    cmdExamine: HandleExamine(G, G.LastNoun);
    cmdTake: HandleTake(G, G.LastNoun);
    cmdDrop: HandleDrop(G, G.LastNoun);
    cmdUse: HandleUse(G, G.LastNoun);
    cmdOpen: HandleOpen(G, G.LastNoun);
    cmdRead: HandleRead(G, G.LastNoun);
    cmdTalk: HandleTalk(G, G.LastNoun);
    cmdInventory: HandleInventory(G);
    cmdUnknown: G.LastMessage := 'I don''t understand that command.';
    cmdNone: ; { Do nothing }
  end;
end;

procedure ShowRoom(var G: TGame);
var
  Idx, I: Integer;
  Room: TRoom;
  Exits, ObjList, MobList: string;
  D: TDirection;
  CurrentY: Integer;
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

  CurrentY := 8;

  { List objects in room }
  ObjList := '';
  for I := 1 to MAX_OBJECTS do
    if G.World.Objects[I].Active and
       (G.World.Objects[I].RoomID = G.World.CurrentRoom) and
       (G.World.Objects[I].CarriedBy = 0) then
    begin
      if ObjList <> '' then ObjList := ObjList + ', ';
      ObjList := ObjList + G.World.Objects[I].Name;
    end;

  if ObjList <> '' then
  begin
    SetColor(LightMagenta, Black);
    WriteAt(1, CurrentY, 'You see: ' + ObjList);
    ResetColor;
    Inc(CurrentY);
  end;

  { List mobs in room }
  MobList := '';
  for I := 1 to MAX_MOBS do
    if G.World.Mobs[I].Active and
       (G.World.Mobs[I].RoomID = G.World.CurrentRoom) then
    begin
      if MobList <> '' then MobList := MobList + '  ';
      MobList := MobList + G.World.Mobs[I].Name + ' is here.';
    end;

  if MobList <> '' then
  begin
    SetColor(LightRed, Black);
    WriteAt(1, CurrentY, MobList);
    ResetColor;
    Inc(CurrentY);
  end;

  Inc(CurrentY);

  { Exits }
  Exits := 'Exits:';
  for D := Low(TDirection) to High(TDirection) do
    if Room.Exits[D] <> DIR_NONE then
      Exits := Exits + ' ' + GetExitName(D);

  if Exits = 'Exits:' then
    Exits := 'Exits: None';

  SetColor(Cyan, Black);
  WriteAt(1, CurrentY, Exits);
  ResetColor;
  Inc(CurrentY, 2);

  { Last message }
  if G.LastMessage <> '' then
  begin
    SetColor(LightGreen, Black);
    WriteAt(1, CurrentY, G.LastMessage);
    ResetColor;
    Inc(CurrentY, 2);
  end;

  { Command prompt }
  WriteAt(1, CurrentY, '> ');
end;

procedure ShowHelp;
begin
  ClearScreen;
  SetColor(Yellow, Black);
  WriteCenter(2, '=== HELP ===');
  ResetColor;

  WriteAt(3, 4, 'Movement:');
  WriteAt(5, 5, 'N, NORTH  - Go north');
  WriteAt(5, 6, 'S, SOUTH  - Go south');
  WriteAt(5, 7, 'E, EAST   - Go east');
  WriteAt(5, 8, 'W, WEST   - Go west');

  WriteAt(3, 10, 'Interactions:');
  WriteAt(5, 11, 'EXAMINE <thing>  - Look at something closely');
  WriteAt(5, 12, 'TAKE <object>    - Pick up an object');
  WriteAt(5, 13, 'DROP <object>    - Drop an object');
  WriteAt(5, 14, 'USE <object>     - Use an object');
  WriteAt(5, 15, 'OPEN <object>    - Open an object');
  WriteAt(5, 16, 'READ <object>    - Read an object');
  WriteAt(5, 17, 'TALK <person>    - Talk to someone');
  WriteAt(5, 18, 'I, INVENTORY     - Show inventory');

  WriteAt(3, 20, 'Other:');
  WriteAt(5, 21, 'L, LOOK   - Look around');
  WriteAt(5, 22, 'H, HELP   - Show this help');
  WriteAt(5, 23, 'Q, QUIT   - Quit the game');

  SetColor(Cyan, Black);
  WriteCenter(25, 'Press any key to continue...');
  ResetColor;
  WaitKey;
end;

procedure HandleExamine(var G: TGame; const Noun: string);
var
  ObjIdx, MobIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Examine what?';
    Exit;
  end;

  { Check objects in room first }
  ObjIdx := FindObjectByName(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx > 0 then
  begin
    G.LastMessage := G.World.Objects[ObjIdx].Desc;
    Exit;
  end;

  { Check player inventory }
  ObjIdx := FindObjectByName(G.World, 0, Noun);
  if ObjIdx > 0 then
  begin
    G.LastMessage := G.World.Objects[ObjIdx].Desc;
    Exit;
  end;

  { Check mobs }
  MobIdx := FindMobByName(G.World, G.World.CurrentRoom, Noun);
  if MobIdx > 0 then
  begin
    G.LastMessage := G.World.Mobs[MobIdx].Desc;
    Exit;
  end;

  G.LastMessage := 'You don''t see that here.';
end;

procedure HandleTake(var G: TGame; const Noun: string);
var
  ObjIdx, I: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Take what?';
    Exit;
  end;

  ObjIdx := FindObjectByName(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t see that here.';
    Exit;
  end;

  if not (ofPickup in G.World.Objects[ObjIdx].Flags) then
  begin
    G.LastMessage := 'You can''t take that.';
    Exit;
  end;

  if G.World.PlayerInvCount >= MAX_INVENTORY then
  begin
    G.LastMessage := 'Your inventory is full.';
    Exit;
  end;

  { Add to inventory }
  Inc(G.World.PlayerInvCount);
  G.World.PlayerInventory[G.World.PlayerInvCount] := G.World.Objects[ObjIdx].ID;
  G.World.Objects[ObjIdx].RoomID := 0;
  G.World.Objects[ObjIdx].CarriedBy := 0;

  G.LastMessage := 'You take the ' + G.World.Objects[ObjIdx].Name + '.';
end;

procedure HandleDrop(var G: TGame; const Noun: string);
var
  ObjIdx, I, InvSlot: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Drop what?';
    Exit;
  end;

  { Find in inventory }
  ObjIdx := FindObjectByName(G.World, 0, Noun);
  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t have that.';
    Exit;
  end;

  { Find inventory slot }
  InvSlot := 0;
  for I := 1 to G.World.PlayerInvCount do
    if G.World.PlayerInventory[I] = G.World.Objects[ObjIdx].ID then
    begin
      InvSlot := I;
      Break;
    end;

  if InvSlot = 0 then
  begin
    G.LastMessage := 'You don''t have that.';
    Exit;
  end;

  { Remove from inventory }
  for I := InvSlot to G.World.PlayerInvCount - 1 do
    G.World.PlayerInventory[I] := G.World.PlayerInventory[I + 1];
  G.World.PlayerInventory[G.World.PlayerInvCount] := 0;
  Dec(G.World.PlayerInvCount);

  { Place in room }
  G.World.Objects[ObjIdx].RoomID := G.World.CurrentRoom;
  G.World.Objects[ObjIdx].CarriedBy := 0;

  G.LastMessage := 'You drop the ' + G.World.Objects[ObjIdx].Name + '.';
end;

procedure HandleUse(var G: TGame; const Noun: string);
var
  ObjIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Use what?';
    Exit;
  end;

  { Check room and inventory }
  ObjIdx := FindObjectByName(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx < 0 then
    ObjIdx := FindObjectByName(G.World, 0, Noun);

  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t see that here.';
    Exit;
  end;

  if not (ofUse in G.World.Objects[ObjIdx].Flags) then
  begin
    G.LastMessage := 'You can''t use that.';
    Exit;
  end;

  if G.World.Objects[ObjIdx].UseText <> '' then
    G.LastMessage := G.World.Objects[ObjIdx].UseText
  else
    G.LastMessage := 'You use the ' + G.World.Objects[ObjIdx].Name + '.';
end;

procedure HandleOpen(var G: TGame; const Noun: string);
var
  ObjIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Open what?';
    Exit;
  end;

  ObjIdx := FindObjectByName(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx < 0 then
    ObjIdx := FindObjectByName(G.World, 0, Noun);

  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t see that here.';
    Exit;
  end;

  if not (ofOpen in G.World.Objects[ObjIdx].Flags) then
  begin
    G.LastMessage := 'You can''t open that.';
    Exit;
  end;

  if G.World.Objects[ObjIdx].UseText <> '' then
    G.LastMessage := G.World.Objects[ObjIdx].UseText
  else
    G.LastMessage := 'You open the ' + G.World.Objects[ObjIdx].Name + '.';
end;

procedure HandleRead(var G: TGame; const Noun: string);
var
  ObjIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Read what?';
    Exit;
  end;

  ObjIdx := FindObjectByName(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx < 0 then
    ObjIdx := FindObjectByName(G.World, 0, Noun);

  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t see that here.';
    Exit;
  end;

  if not (ofRead in G.World.Objects[ObjIdx].Flags) then
  begin
    G.LastMessage := 'There''s nothing to read on that.';
    Exit;
  end;

  if G.World.Objects[ObjIdx].UseText <> '' then
    G.LastMessage := G.World.Objects[ObjIdx].UseText
  else
    G.LastMessage := 'You read the ' + G.World.Objects[ObjIdx].Name + '.';
end;

procedure HandleTalk(var G: TGame; const Noun: string);
var
  MobIdx: Integer;
  NounToSearch: string;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Talk to whom?';
    Exit;
  end;

  { Handle "TALK TO <noun>" }
  NounToSearch := Noun;
  if Pos('TO ', UpperCase(NounToSearch)) = 1 then
    NounToSearch := Trim(Copy(NounToSearch, 4, Length(NounToSearch)));

  MobIdx := FindMobByName(G.World, G.World.CurrentRoom, NounToSearch);
  if MobIdx < 0 then
  begin
    G.LastMessage := 'There''s no one here by that name.';
    Exit;
  end;

  if G.World.Mobs[MobIdx].Dialogue <> '' then
    G.LastMessage := G.World.Mobs[MobIdx].Name + ' says: "' +
                     G.World.Mobs[MobIdx].Dialogue + '"'
  else
    G.LastMessage := G.World.Mobs[MobIdx].Name + ' has nothing to say.';
end;

procedure HandleInventory(var G: TGame);
var
  I, ObjIdx: Integer;
  InvList: string;
begin
  if G.World.PlayerInvCount = 0 then
  begin
    G.LastMessage := 'You are carrying nothing.';
    Exit;
  end;

  InvList := 'You are carrying: ';
  for I := 1 to G.World.PlayerInvCount do
  begin
    ObjIdx := FindObjectByID(G.World, G.World.PlayerInventory[I]);
    if ObjIdx > 0 then
    begin
      if I > 1 then InvList := InvList + ', ';
      InvList := InvList + G.World.Objects[ObjIdx].Name;
    end;
  end;

  G.LastMessage := InvList;
end;

procedure RunGame(var G: TGame);
var
  Input: string;
  Cmd: TCommandType;
  PromptY: Integer;
begin
  InitDisplay;

  while G.State = gsPlaying do
  begin
    ShowRoom(G);
    { Find prompt position - scan for '>' }
    PromptY := 14;
    Input := ReadLine(3, PromptY, 60);
    Cmd := ParseCommand(Input, G.LastNoun);
    ExecuteCommand(G, Cmd);
  end;

  ClearScreen;
  WriteCenter(12, 'Thanks for playing!');
  Delay(1500);
end;

end.
