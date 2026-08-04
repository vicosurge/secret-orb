{ gamecore.pas - Core game engine for Secret Orb }
unit GameCore;

{$MODE OBJFPC}

interface

uses
  Crt, SysUtils, GameData, DataFile, Display, Events;

type
  TCommandType = (
    cmdNone,
    cmdNorth, cmdSouth, cmdEast, cmdWest, cmdUp, cmdDown,
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
    cmdGive,
    cmdInventory,
    cmdSave,
    cmdLoad,
    cmdScore,
    cmdExits,
    cmdAgain
  );

  TGameState = (gsPlaying, gsQuit, gsWon);

  TGame = record
    World: TGameWorld;
    State: TGameState;
    LastMessage: string;
    LastNoun: string[40];
    { The second noun of USE X ON Y and GIVE X TO Y, empty for every other
      command. Only those two verbs are split on a preposition - see
      SplitPrep - so TALK TO WIZARD keeps its noun in one piece. }
    LastNoun2: string[40];
    PromptY: Integer;       { Set by ShowRoom so input lands on the drawn prompt }
    SaveFile: string;
    { What AGAIN repeats. Only turn-consuming commands are recorded, so AGAIN
      never repeats itself and never replays a save or a help screen. Both
      nouns are kept, or AGAIN after USE KEY ON DOOR would silently replay
      the bare USE KEY. }
    PrevCmd: TCommandType;
    PrevNoun: string[40];
    PrevNoun2: string[40];
    { What the events fired by the command in hand want to say or do. Filled
      by the hooks in the handlers, drained by RunGame at a scene boundary -
      a paragraph clears the screen, so it cannot be shown from inside a
      handler that is still building LastMessage. }
    Pending: TEventOutcome;
  end;

procedure InitGame(var G: TGame);
function LoadGame(var G: TGame; const FileName: string): Boolean;
procedure RunGame(var G: TGame);
function ParseCommand(const Input: string; var Noun, Noun2: string): TCommandType;
procedure ExecuteCommand(var G: TGame; Cmd: TCommandType);
procedure ShowRoom(var G: TGame);
procedure ShowHelp;
procedure HandleExamine(var G: TGame; const Noun: string);
procedure HandleTake(var G: TGame; const Noun: string);
procedure HandleDrop(var G: TGame; const Noun: string);
procedure HandleUse(var G: TGame; const Noun, Noun2: string);
procedure HandleGive(var G: TGame; const Noun, Noun2: string);
procedure HandleOpen(var G: TGame; const Noun: string);
procedure HandleRead(var G: TGame; const Noun: string);
procedure HandleTalk(var G: TGame; const Noun: string);
procedure HandleInventory(var G: TGame);
procedure HandleSave(var G: TGame; const Noun: string);
procedure HandleLoad(var G: TGame; const Noun: string);
procedure HandleExits(var G: TGame);
procedure HandleScore(var G: TGame);

const
  DEFAULT_SAVE = 'SAVE.DAT';   { 8.3 so it works on DOS }

implementation

procedure InitGame(var G: TGame);
begin
  InitWorld(G.World);
  G.State := gsPlaying;
  G.LastMessage := '';
  G.LastNoun := '';
  G.LastNoun2 := '';
  G.PromptY := 14;
  G.SaveFile := DEFAULT_SAVE;
  G.PrevCmd := cmdNone;
  G.PrevNoun := '';
  G.PrevNoun2 := '';
  ClearOutcome(G.Pending);
end;

function LoadGame(var G: TGame; const FileName: string): Boolean;
begin
  InitGame(G);
  Result := LoadWorld(FileName, G.World);
end;

{ Every story paragraph reaches the player through here, so a world that opts
  into booklet mode cites numbers everywhere rather than in most places. }
procedure ShowParagraph(var G: TGame; Num: Word);
var
  Body: TParaText;
begin
  if Num = 0 then Exit;
  Body := ParagraphText(G.World, Num);
  if Body = '' then Exit;

  if (G.World.WorldFlags and WF_BOOKLET) <> 0 then
  begin
    ClearScreen;
    DrawBox(18, 10, 62, 14);
    SetColor(Yellow, Black);
    WriteCenter(12, 'Read paragraph ' + IntToStr(Num) + ' in your booklet.');
    ResetColor;
    SetColor(Cyan, Black);
    WriteCenter(SCREEN_HEIGHT, PRESS_ANY_KEY);
    ResetColor;
    WaitKey;
  end
  else
    ShowTextPage(G.World.Title, Body);
end;

{ Declared before its first use. It used to sit below ParseCommand, which meant
  the calls in there bound SysUtils.Trim while every later call in the unit
  bound this one - a change here would have applied to only some call sites. }
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

{ Splits "KEY ON DOOR" into "KEY" and "DOOR" at the first occurrence of Prep.
  Only USE and GIVE go through here. Splitting every noun would break
  TALK TO WIZARD, whose noun is one thing with a preposition glued to the
  front - and DROP THE TORCH ON THE FLOOR, which names one object. The
  search is on an upper-cased copy, which is the same length, so the
  positions still index the original and the noun keeps its typed case. }
function SplitPrep(var Noun, Noun2: string; const Prep: string): Boolean;
var
  P: Integer;
begin
  Result := False;
  P := Pos(Prep, UpperCase(Noun));
  if P <= 0 then Exit;
  Noun2 := Trim(Copy(Noun, P + Length(Prep), Length(Noun)));
  Noun := Trim(Copy(Noun, 1, P - 1));
  Result := True;
end;

function ParseCommand(const Input: string; var Noun, Noun2: string): TCommandType;
var
  Cmd, FullCmd: string;
  SpacePos: Integer;
begin
  FullCmd := Trim(Input);
  Noun := '';
  Noun2 := '';

  { Find first word. Only the verb is upper-cased - the noun keeps the case it
    was typed in, because SAVE and LOAD take a file name. }
  SpacePos := Pos(' ', FullCmd);
  if SpacePos > 0 then
  begin
    Cmd := UpperCase(Copy(FullCmd, 1, SpacePos - 1));
    Noun := Trim(Copy(FullCmd, SpacePos + 1, Length(FullCmd)));
    { Handle "LOOK AT" as EXAMINE }
    if (Cmd = 'LOOK') and (Pos('AT ', UpperCase(Noun)) = 1) then
    begin
      Noun := Trim(Copy(Noun, 4, Length(Noun)));
      Result := cmdExamine;
      Exit;
    end;
  end
  else
    Cmd := UpperCase(FullCmd);

  if (Cmd = 'N') or (Cmd = 'NORTH') then
    Result := cmdNorth
  else if (Cmd = 'S') or (Cmd = 'SOUTH') then
    Result := cmdSouth
  else if (Cmd = 'E') or (Cmd = 'EAST') then
    Result := cmdEast
  else if (Cmd = 'W') or (Cmd = 'WEST') then
    Result := cmdWest
  else if (Cmd = 'U') or (Cmd = 'UP') then
    Result := cmdUp
  else if (Cmd = 'D') or (Cmd = 'DOWN') then
    Result := cmdDown
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
  else if (Cmd = 'GIVE') or (Cmd = 'OFFER') then
    Result := cmdGive
  else if (Cmd = 'I') or (Cmd = 'INV') or (Cmd = 'INVENTORY') then
    Result := cmdInventory
  else if Cmd = 'SAVE' then
    Result := cmdSave
  else if (Cmd = 'LOAD') or (Cmd = 'RESTORE') then
    Result := cmdLoad
  else if Cmd = 'SCORE' then
    Result := cmdScore
  else if Cmd = 'EXITS' then
    Result := cmdExits
  else if (Cmd = 'G') or (Cmd = 'AGAIN') then
    Result := cmdAgain
  else if Cmd = '' then
    Result := cmdNone
  else
    Result := cmdUnknown;

  { The only two verbs that take a second object. ON and WITH are the same
    thing to the engine; an author writes one event and both spellings reach
    it. TO is accepted for USE as well, because USE COIN TO SLOT reads
    naturally enough that a player will try it. }
  case Result of
    cmdUse:
      if not SplitPrep(Noun, Noun2, ' ON ') then
        if not SplitPrep(Noun, Noun2, ' WITH ') then
          SplitPrep(Noun, Noun2, ' TO ');
    cmdGive:
      if not SplitPrep(Noun, Noun2, ' TO ') then
        SplitPrep(Noun, Noun2, ' ON ');
  else
    ;
  end;
end;

{ Every trigger site goes through here, so the queue is the only way an event
  can reach the player and there is one place to put a breakpoint. }
procedure Fire(var G: TGame; Trig: TEventTrigger; ID1, ID2: Word);
begin
  FireEvents(G.World, Trig, ID1, ID2, G.Pending);
end;

{ Everything that happens on arriving in a room: the visited bitmap, the
  first-visit points and the arrival scene, plus the enter and first-visit
  triggers. MovePlayer, the start-room block in RunGame and a teleporting
  event all come through here, so no caller can miss half of it - which is
  what used to make Room.FirstVisitPara a trigger with two separate sites.
  Returns the points awarded, because MovePlayer shows them and the start
  room does not. }
function EnterRoom(var G: TGame; TargetIdx: Integer): Word;
begin
  Result := 0;
  Fire(G, etEnterRoom, G.World.Rooms[TargetIdx].ID, 0);

  if not G.World.Visited[TargetIdx] then
  begin
    G.World.Visited[TargetIdx] := True;
    Result := G.World.Rooms[TargetIdx].Points;
    Inc(G.World.Score, Result);
    ShowParagraph(G, G.World.Rooms[TargetIdx].FirstVisitPara);
    Fire(G, etFirstVisit, G.World.Rooms[TargetIdx].ID, 0);
  end;
end;

procedure MovePlayer(var G: TGame; Dir: TDirection);
var
  Idx, TargetID, TargetIdx: Integer;
  Points: Word;
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

  { Fired before the move, with the room being left, so a condition of
    ctRoomIs in the event still names the room the player is walking out of }
  Fire(G, etExitRoom, G.World.Rooms[Idx].ID, TargetID);

  G.World.CurrentRoom := TargetID;
  G.LastMessage := 'You go ' + GetExitName(Dir) + '.';

  Points := EnterRoom(G, TargetIdx);
  if Points > 0 then
    G.LastMessage := G.LastMessage + ' [+' + IntToStr(Points) + ']';
end;

{ The world is won by reaching WinRoomID, optionally while carrying WinObjectID }
procedure CheckWinCondition(var G: TGame);
begin
  if G.World.WinRoomID = 0 then Exit;
  if G.World.CurrentRoom <> G.World.WinRoomID then Exit;
  if (G.World.WinObjectID <> 0) and
     not PlayerHasObject(G.World, G.World.WinObjectID) then Exit;
  G.State := gsWon;
end;

{ The one place exits are turned into prose - ShowRoom draws it every turn and
  the EXITS command reports the same string. }
function ExitsLine(var G: TGame): string;
var
  Idx: Integer;
  D: TDirection;
begin
  Result := 'Exits:';
  Idx := FindRoomByID(G.World, G.World.CurrentRoom);
  if Idx > 0 then
    for D := Low(TDirection) to High(TDirection) do
      if G.World.Rooms[Idx].Exits[D] <> DIR_NONE then
        Result := Result + ' ' + GetExitName(D);
  if Result = 'Exits:' then
    Result := 'Exits: None';
end;

procedure HandleExits(var G: TGame);
begin
  G.LastMessage := ExitsLine(G);
end;

procedure HandleScore(var G: TGame);
begin
  G.LastMessage := 'Score: ' + IntToStr(G.World.Score);
  if G.World.MaxScore > 0 then
    G.LastMessage := G.LastMessage + ' of ' + IntToStr(G.World.MaxScore);
  G.LastMessage := G.LastMessage + '   Turns: ' + IntToStr(G.World.Turns);
end;

procedure HandleSave(var G: TGame; const Noun: string);
begin
  if Noun <> '' then
    G.SaveFile := Noun;
  if SaveGameState(G.SaveFile, G.World) then
    G.LastMessage := 'Game saved to ' + G.SaveFile + '.'
  else
    G.LastMessage := 'Could not save to ' + G.SaveFile + '.';
end;

procedure HandleLoad(var G: TGame; const Noun: string);
begin
  if Noun <> '' then
    G.SaveFile := Noun;
  if LoadGameState(G.SaveFile, G.World) then
    G.LastMessage := 'Game restored from ' + G.SaveFile + '.'
  else
    G.LastMessage := 'No usable save found in ' + G.SaveFile + '.';
end;

procedure ExecuteCommand(var G: TGame; Cmd: TCommandType);
begin
  { AGAIN is resolved before dispatch, so the replayed command behaves in every
    respect - including the turn count - as if the player had retyped it. }
  if Cmd = cmdAgain then
  begin
    if G.PrevCmd = cmdNone then
    begin
      G.LastMessage := 'There is nothing to repeat.';
      Exit;
    end;
    Cmd := G.PrevCmd;
    G.LastNoun := G.PrevNoun;
    G.LastNoun2 := G.PrevNoun2;
  end;

  case Cmd of
    cmdNorth: MovePlayer(G, dirNorth);
    cmdSouth: MovePlayer(G, dirSouth);
    cmdEast:  MovePlayer(G, dirEast);
    cmdWest:  MovePlayer(G, dirWest);
    cmdUp:    MovePlayer(G, dirUp);
    cmdDown:  MovePlayer(G, dirDown);
    cmdLook:  G.LastMessage := '';
    cmdHelp:  ShowHelp;
    cmdQuit:  G.State := gsQuit;
    cmdExamine: HandleExamine(G, G.LastNoun);
    cmdTake: HandleTake(G, G.LastNoun);
    cmdDrop: HandleDrop(G, G.LastNoun);
    cmdUse: HandleUse(G, G.LastNoun, G.LastNoun2);
    cmdOpen: HandleOpen(G, G.LastNoun);
    cmdRead: HandleRead(G, G.LastNoun);
    cmdTalk: HandleTalk(G, G.LastNoun);
    cmdGive: HandleGive(G, G.LastNoun, G.LastNoun2);
    cmdInventory: HandleInventory(G);
    cmdSave: HandleSave(G, G.LastNoun);
    cmdLoad: HandleLoad(G, G.LastNoun);
    cmdScore: HandleScore(G);
    cmdExits: HandleExits(G);
    cmdUnknown: G.LastMessage := 'I don''t understand that command.';
    cmdNone: ; { Do nothing }
  end;

  { Only commands that act on the world consume a turn - asking for the score or
    saving the game should not inflate the turn count. The same set is what
    AGAIN will repeat, which is why cmdAgain cannot end up recorded here: by
    this point it has already been rewritten to the command it stood for. }
  case Cmd of
    cmdNone, cmdUnknown, cmdHelp, cmdScore, cmdSave, cmdLoad, cmdQuit,
    cmdExits: ;
  else
    begin
      Inc(G.World.Turns);
      G.PrevCmd := Cmd;
      G.PrevNoun := G.LastNoun;
      G.PrevNoun2 := G.LastNoun2;
      { Inside this branch on purpose: a timer inherits the meta-command
        exclusion above for free, so HELP, SCORE, SAVE, LOAD and EXITS do not
        advance the clock. That is the whole argument for hooking here rather
        than in RunGame's loop. }
      Fire(G, etTimer, 0, 0);
    end;
  end;

  if G.State = gsPlaying then
    CheckWinCondition(G);
end;

procedure ShowRoom(var G: TGame);
var
  Idx, I: Integer;
  Room: TRoom;
  Exits, ObjList, MobList: string;
  CurrentY, Avail, Lines: Integer;
begin
  ClearScreen;

  Idx := FindRoomByID(G.World, G.World.CurrentRoom);
  if Idx < 0 then
  begin
    WriteAt(1, 1, 'ERROR: Room not found!');
    WriteAt(1, 3, '> ');
    G.PromptY := 3;
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
  Exits := ExitsLine(G);

  SetColor(Cyan, Black);
  WriteAt(1, CurrentY, Exits);
  ResetColor;
  Inc(CurrentY, 2);

  { Last message. Wrapped rather than written as one line: an examine puts a
    whole object description here, and letting the terminal do the wrapping
    pushed the prompt off the row recorded in PromptY. Bounded so that the
    blank line and the prompt below always still fit on the screen. }
  if G.LastMessage <> '' then
  begin
    Avail := SCREEN_HEIGHT - 1 - CurrentY;
    if Avail < 1 then Avail := 1;
    SetColor(LightGreen, Black);
    Lines := WriteWrappedMax(1, CurrentY, SCREEN_WIDTH - 2, Avail,
                             G.LastMessage);
    ResetColor;
    Inc(CurrentY, Lines + 1);
  end;

  { Command prompt - remember where it landed so RunGame reads input there }
  if CurrentY > SCREEN_HEIGHT then
    CurrentY := SCREEN_HEIGHT;
  WriteAt(1, CurrentY, '> ');
  G.PromptY := CurrentY;
end;

procedure ShowHelp;
begin
  ClearScreen;
  SetColor(Yellow, Black);
  WriteCenter(2, '=== HELP ===');
  ResetColor;

  { Two columns, so everything fits within the 25 available lines }
  WriteAt(3, 4, 'Movement:');
  WriteAt(5, 5, 'N, NORTH  - Go north');
  WriteAt(5, 6, 'S, SOUTH  - Go south');
  WriteAt(5, 7, 'E, EAST   - Go east');
  WriteAt(5, 8, 'W, WEST   - Go west');
  WriteAt(5, 9, 'U, UP     - Go up');
  WriteAt(5, 10, 'D, DOWN   - Go down');

  WriteAt(3, 12, 'Interactions:');
  WriteAt(5, 13, 'EXAMINE <thing>  - Look at something closely');
  WriteAt(5, 14, 'TAKE <object>    - Pick up an object');
  WriteAt(5, 15, 'DROP <object>    - Drop an object');
  WriteAt(5, 16, 'USE <object>     - Use an object');
  WriteAt(5, 17, 'USE <a> ON <b>   - Use one thing on another');
  WriteAt(5, 18, 'OPEN <object>    - Open an object');
  WriteAt(5, 19, 'READ <object>    - Read an object');
  WriteAt(5, 20, 'TALK <person>    - Talk to someone');
  WriteAt(5, 21, 'GIVE <a> TO <b>  - Offer something to someone');
  WriteAt(5, 22, 'I, INVENTORY     - Show inventory');

  WriteAt(43, 4, 'Other:');
  WriteAt(45, 5, 'L, LOOK   - Look around');
  WriteAt(45, 6, 'EXITS     - List the ways out');
  WriteAt(45, 7, 'G, AGAIN  - Repeat last command');
  WriteAt(45, 8, 'H, HELP   - Show this help');
  WriteAt(45, 9, 'Q, QUIT   - Quit the game');

  WriteAt(43, 11, 'Progress:');
  WriteAt(45, 12, 'SCORE     - Show score and turns');
  WriteAt(45, 13, 'SAVE [file]    - Save your game');
  WriteAt(45, 14, 'LOAD [file]    - Restore a game');
  WriteAt(45, 16, 'Default save file: ' + DEFAULT_SAVE);

  SetColor(Cyan, Black);
  WriteCenter(24, PRESS_ANY_KEY);
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

  { Anything in the room or in hand }
  ObjIdx := FindObjectVisible(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx > 0 then
  begin
    G.LastMessage := G.World.Objects[ObjIdx].Desc;
    Fire(G, etExamineObject, G.World.Objects[ObjIdx].ID, 0);
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
  ObjIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Take what?';
    Exit;
  end;

  { Room only - an item already in hand must not be taken a second time }
  ObjIdx := FindObjectInRoom(G.World, G.World.CurrentRoom, Noun);
  if ObjIdx < 0 then
  begin
    if FindObjectInInventory(G.World, Noun) > 0 then
      G.LastMessage := 'You already have that.'
    else
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

  { Award points on the first take only, so drop-and-retake cannot farm score
    or replay the scene }
  if not G.World.Taken[ObjIdx] then
  begin
    G.World.Taken[ObjIdx] := True;
    if G.World.Objects[ObjIdx].Points > 0 then
    begin
      Inc(G.World.Score, G.World.Objects[ObjIdx].Points);
      G.LastMessage := G.LastMessage + ' [+' +
                       IntToStr(G.World.Objects[ObjIdx].Points) + ']';
    end;
    ShowParagraph(G, G.World.Objects[ObjIdx].FirstTakePara);
  end;

  Fire(G, etTakeObject, G.World.Objects[ObjIdx].ID, 0);
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
  ObjIdx := FindObjectInInventory(G.World, Noun);
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

  Fire(G, etDropObject, G.World.Objects[ObjIdx].ID, 0);
end;

procedure HandleUse(var G: TGame; const Noun, Noun2: string);
var
  ObjIdx, TgtIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Use what?';
    Exit;
  end;

  { Check room and inventory }
  ObjIdx := FindObjectVisible(G.World, G.World.CurrentRoom, Noun);

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

  { USE X ON Y. The second target is always an object - handing something to
    a person is GIVE, which has its own trigger - so an event's TriggerID2
    names an object and the validator can check it as one. }
  if Noun2 <> '' then
  begin
    TgtIdx := FindObjectVisible(G.World, G.World.CurrentRoom, Noun2);
    if TgtIdx < 0 then
    begin
      if FindMobByName(G.World, G.World.CurrentRoom, Noun2) > 0 then
        G.LastMessage := 'Try giving it to them instead.'
      else
        G.LastMessage := 'You don''t see that here.';
      Exit;
    end;

    { No UseText: that line describes using the object on its own, and
      printing it here would contradict whatever the event goes on to say.
      An event with nothing to say leaves the flat report standing. }
    G.LastMessage := 'You use the ' + G.World.Objects[ObjIdx].Name +
                     ' on the ' + G.World.Objects[TgtIdx].Name + '.';
    Fire(G, etUseObjectOn, G.World.Objects[ObjIdx].ID,
         G.World.Objects[TgtIdx].ID);
    Exit;
  end;

  if G.World.Objects[ObjIdx].UseText <> '' then
    G.LastMessage := G.World.Objects[ObjIdx].UseText
  else
    G.LastMessage := 'You use the ' + G.World.Objects[ObjIdx].Name + '.';

  Fire(G, etUseObject, G.World.Objects[ObjIdx].ID, 0);
end;

{ GIVE moves nothing by itself. What handing an item over means is the
  author's decision - the mob may take it, swap it, or refuse - so the
  transfer belongs in the event's actions. Without an event the object stays
  in the player's hands, which is the safe way to be wrong: an item silently
  swallowed by an NPC nobody wrote a response for could strand the game. }
procedure HandleGive(var G: TGame; const Noun, Noun2: string);
var
  ObjIdx, MobIdx: Integer;
begin
  if Noun = '' then
  begin
    G.LastMessage := 'Give what?';
    Exit;
  end;

  { Inventory only - you cannot hand over what you are not holding }
  ObjIdx := FindObjectInInventory(G.World, Noun);
  if ObjIdx < 0 then
  begin
    G.LastMessage := 'You don''t have that.';
    Exit;
  end;

  if Noun2 = '' then
  begin
    G.LastMessage := 'Give the ' + G.World.Objects[ObjIdx].Name + ' to whom?';
    Exit;
  end;

  MobIdx := FindMobByName(G.World, G.World.CurrentRoom, Noun2);
  if MobIdx < 0 then
  begin
    G.LastMessage := 'There''s no one here by that name.';
    Exit;
  end;

  Fire(G, etGiveTo, G.World.Objects[ObjIdx].ID, G.World.Mobs[MobIdx].ID);

  { Left empty when an event answered, so its message stands on its own
    rather than following a refusal the author has just overridden }
  if HasOutcome(G.Pending) then
    G.LastMessage := ''
  else
    G.LastMessage := G.World.Mobs[MobIdx].Name + ' does not want the ' +
                     G.World.Objects[ObjIdx].Name + '.';
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

  ObjIdx := FindObjectVisible(G.World, G.World.CurrentRoom, Noun);

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

  ObjIdx := FindObjectVisible(G.World, G.World.CurrentRoom, Noun);

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

  { The opening scene plays once; after that only the dialogue line remains }
  if not G.World.Talked[MobIdx] then
  begin
    G.World.Talked[MobIdx] := True;
    ShowParagraph(G, G.World.Mobs[MobIdx].FirstTalkPara);
  end;

  Fire(G, etTalkToMob, G.World.Mobs[MobIdx].ID, 0);
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

procedure ShowEnding(var G: TGame);
begin
  ShowParagraph(G, G.World.WinPara);

  ClearScreen;
  SetColor(Yellow, Black);
  WriteCenter(8, '*** YOU HAVE WON ***');
  ResetColor;
  WriteCenter(10, G.World.Title);
  if G.World.MaxScore > 0 then
    WriteCenter(12, 'Final score: ' + IntToStr(G.World.Score) + ' of ' +
                    IntToStr(G.World.MaxScore))
  else
    WriteCenter(12, 'Final score: ' + IntToStr(G.World.Score));
  WriteCenter(13, 'Turns taken: ' + IntToStr(G.World.Turns));
  SetColor(Cyan, Black);
  WriteCenter(16, PRESS_ANY_KEY);
  ResetColor;
  WaitKey;
end;

{ Drains what the events fired this turn want to do. Called from RunGame
  between the command and the next room draw - a scene boundary, because
  ShowParagraph clears the screen and LastMessage has not been drawn yet.
  Paragraphs go through the same ShowParagraph as the six built-in story
  triggers, so booklet mode needs no special case here. }
procedure ApplyOutcome(var G: TGame);
var
  I: Integer;
  Idx: Integer;
  O: TEventOutcome;
begin
  if not HasOutcome(G.Pending) then Exit;

  { Taken by value and cleared first: a teleport re-enters EnterRoom, which
    can fire more events into G.Pending, and those belong to the next drain
    rather than to the one already in progress. }
  O := G.Pending;
  ClearOutcome(G.Pending);

  if O.Message <> '' then
  begin
    if G.LastMessage = '' then
      G.LastMessage := O.Message
    else
      G.LastMessage := G.LastMessage + ' ' + O.Message;
  end;

  if O.Points > 0 then
  begin
    Inc(G.World.Score, O.Points);
    G.LastMessage := G.LastMessage + ' [+' + IntToStr(O.Points) + ']';
  end;

  for I := 1 to O.ParaCount do
    ShowParagraph(G, O.Paras[I]);

  if O.Teleport <> 0 then
  begin
    Idx := FindRoomByID(G.World, O.Teleport);
    if Idx > 0 then
    begin
      G.World.CurrentRoom := O.Teleport;
      EnterRoom(G, Idx);
    end;
  end;

  case O.Ending of
    ekWin:  G.State := gsWon;
    ekLose: G.State := gsQuit;
    ekNone: ;
  end;
end;


procedure RunGame(var G: TGame);
var
  Input: string;
  Cmd: TCommandType;
  StartIdx: Integer;
begin
  InitDisplay;

  ShowParagraph(G, G.World.IntroPara);

  { The starting room counts as visited, and scores like any other. It never
    goes through HandleMove, so its arrival scene has to be played here. }
  StartIdx := FindRoomByID(G.World, G.World.CurrentRoom);
  if StartIdx > 0 then
    EnterRoom(G, StartIdx);
  ApplyOutcome(G);

  while G.State = gsPlaying do
  begin
    ShowRoom(G);
    Input := ReadLine(3, G.PromptY, 60);
    Cmd := ParseCommand(Input, G.LastNoun, G.LastNoun2);
    ExecuteCommand(G, Cmd);
    ApplyOutcome(G);
  end;

  if G.State = gsWon then
    ShowEnding(G)
  else
    { Ending the adventure without winning is its own ending }
    ShowParagraph(G, G.World.LosePara);

  ClearScreen;
  WriteCenter(12, 'Thanks for playing!');
  Delay(1500);
end;

end.
