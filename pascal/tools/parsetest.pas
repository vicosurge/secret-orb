{ parsetest.pas - Unit tests for the two-noun command parser and the two
  triggers that depend on it, etUseObjectOn and etGiveTo.

  Native only, unlike pairtest and eventtest. This program links GameCore,
  which uses Crt; on DOS that writes to video memory and reads the BIOS
  keyboard, so nothing here could be captured through a pipe on FreeDOS. On
  Linux Crt falls back to plain writes when stdout is not a terminal, which
  is what makes the test runnable at all. Nothing it exercises is
  target-specific, so the DOS run losing it costs little - but it is why
  make dos32 does not build it. }
program ParseTest;

{$MODE OBJFPC}

uses
  SysUtils, GameData, GameCore, Events;

var
  Failures: Integer;

procedure CheckStr(const What, Got, Want: string);
begin
  if Got = Want then
    WriteLn('  ok    ', What, ' = "', Got, '"')
  else
  begin
    WriteLn('  FAIL  ', What, ' = "', Got, '", expected "', Want, '"');
    Inc(Failures);
  end;
end;

procedure CheckInt(const What: string; Got, Want: Integer);
begin
  if Got = Want then
    WriteLn('  ok    ', What, ' = ', Got)
  else
  begin
    WriteLn('  FAIL  ', What, ' = ', Got, ', expected ', Want);
    Inc(Failures);
  end;
end;

procedure Check(const What: string; Got: Boolean);
begin
  if Got then
    WriteLn('  ok    ', What)
  else
  begin
    WriteLn('  FAIL  ', What);
    Inc(Failures);
  end;
end;

{ Parses one line and checks all three results at once, which is most of what
  this file has to say }
procedure CheckParse(const Input: string; WantCmd: TCommandType;
                     const WantNoun, WantNoun2: string);
var
  Cmd: TCommandType;
  N1, N2: string;
begin
  Cmd := ParseCommand(Input, N1, N2);
  CheckInt('"' + Input + '" verb', Ord(Cmd), Ord(WantCmd));
  CheckStr('"' + Input + '" noun', N1, WantNoun);
  CheckStr('"' + Input + '" noun2', N2, WantNoun2);
end;

procedure TestPrepositions;
begin
  WriteLn('Splitting USE and GIVE on a preposition:');

  CheckParse('USE KEY ON DOOR', cmdUse, 'KEY', 'DOOR');
  CheckParse('USE KEY WITH DOOR', cmdUse, 'KEY', 'DOOR');
  CheckParse('USE COIN TO SLOT', cmdUse, 'COIN', 'SLOT');
  CheckParse('GIVE COIN TO MERCHANT', cmdGive, 'COIN', 'MERCHANT');
  CheckParse('OFFER COIN TO MERCHANT', cmdGive, 'COIN', 'MERCHANT');

  { The verb is upper-cased, the nouns are not - SAVE and LOAD take file
    names on case-sensitive filesystems, and the object lookup is already
    case-insensitive }
  CheckParse('use key on door', cmdUse, 'key', 'door');

  { Extra spacing around the preposition is the player's, not the author's }
  CheckParse('USE  KEY  ON  DOOR', cmdUse, 'KEY', 'DOOR');

  { One noun still parses as one noun }
  CheckParse('USE LAMP', cmdUse, 'LAMP', '');
  CheckParse('GIVE COIN', cmdGive, 'COIN', '');
end;

procedure TestOnlyTwoVerbsSplit;
begin
  WriteLn('Verbs that must not be split:');

  { TALK TO WIZARD is one noun with a preposition glued to the front, which
    HandleTalk strips itself. Splitting on TO everywhere would leave the
    verb with no noun at all. }
  CheckParse('TALK TO WIZARD', cmdTalk, 'TO WIZARD', '');

  { DROP names one object even when the phrasing looks like two }
  CheckParse('DROP TORCH ON FLOOR', cmdDrop, 'TORCH ON FLOOR', '');
  CheckParse('EXAMINE PAINTING ON WALL', cmdExamine,
             'PAINTING ON WALL', '');
  CheckParse('LOOK AT PAINTING ON WALL', cmdExamine,
             'PAINTING ON WALL', '');

  { A file name may contain the letters, and does not become two nouns }
  CheckParse('SAVE game to keep.dat', cmdSave, 'game to keep.dat', '');
end;

procedure TestNoun2IsCleared;
var
  Cmd: TCommandType;
  N1, N2: string;
begin
  WriteLn('The second noun does not survive the next command:');

  Cmd := ParseCommand('USE KEY ON DOOR', N1, N2);
  CheckStr('first parse leaves a second noun', N2, 'DOOR');
  Cmd := ParseCommand('USE LAMP', N1, N2);
  CheckStr('the next parse clears it', N2, '');
  Cmd := ParseCommand('LOOK', N1, N2);
  CheckStr('and a verb with no noun clears it', N2, '');
  CheckInt('LOOK still parses', Ord(Cmd), Ord(cmdLook));
end;

{ A room, a key in hand, a door to use it on, and a merchant to give it to }
procedure BuildWorld(var G: TGame);
begin
  InitGame(G);
  G.World.Title := 'Parse Test World';
  G.World.CurrentRoom := 1;

  G.World.RoomCount := 1;
  G.World.Rooms[1].ID := 1;
  G.World.Rooms[1].Name := 'Hall';
  G.World.Rooms[1].Desc := 'A hall.';
  G.World.Rooms[1].Active := True;

  G.World.ObjectCount := 2;
  G.World.Objects[1].ID := 1;
  G.World.Objects[1].Name := 'Key';
  G.World.Objects[1].Desc := 'A brass key.';
  G.World.Objects[1].RoomID := 0;
  G.World.Objects[1].Flags := [ofPickup, ofUse];
  G.World.Objects[1].Active := True;
  G.World.Objects[2].ID := 2;
  G.World.Objects[2].Name := 'Door';
  G.World.Objects[2].Desc := 'A locked door.';
  G.World.Objects[2].RoomID := 1;
  G.World.Objects[2].Active := True;

  G.World.MobCount := 1;
  G.World.Mobs[1].ID := 1;
  G.World.Mobs[1].Name := 'Merchant';
  G.World.Mobs[1].Desc := 'A merchant.';
  G.World.Mobs[1].RoomID := 1;
  G.World.Mobs[1].Active := True;

  { The key starts in the player's hands }
  G.World.PlayerInvCount := 1;
  G.World.PlayerInventory[1] := 1;
end;

{ Counts how often it fires, so a test can tell one firing from two }
procedure AddCountingEvent(var G: TGame; Slot: Word; Trig: TEventTrigger;
                           ID1, ID2: Word);
begin
  InitEvent(G.World.Events[Slot]);
  with G.World.Events[Slot] do
  begin
    Name := 'counter';
    TriggerType := Trig;
    TriggerID := ID1;
    TriggerID2 := ID2;
    ActionCount := 1;
    Actions[1].ActionType := atAddCounter;
    Actions[1].TargetID := 1;
    Actions[1].Value := 1;
    OneShot := False;   { InitEvent defaults it on; here we count repeats }
    Active := True;
  end;
  if G.World.EventCount < Slot then G.World.EventCount := Slot;
  SeedEventState(G.World);
end;

procedure TestUseObjectOn;
var
  G: TGame;
begin
  WriteLn('etUseObjectOn:');

  BuildWorld(G);
  AddCountingEvent(G, 1, etUseObjectOn, 1, 2);

  G.LastNoun := 'Key';
  G.LastNoun2 := 'Door';
  ExecuteCommand(G, cmdUse);
  CheckInt('firing on the right pair', G.World.Counters[1], 1);
  Check('the flat report stands when the event says nothing',
        Pos('on the Door', G.LastMessage) > 0);

  { The same key on nothing in particular is a different trigger }
  G.World.Counters[1] := 0;
  G.LastNoun := 'Key';
  G.LastNoun2 := '';
  ExecuteCommand(G, cmdUse);
  CheckInt('a bare USE does not fire it', G.World.Counters[1], 0);

  { Wrong second object, same first }
  BuildWorld(G);
  AddCountingEvent(G, 1, etUseObjectOn, 1, 99);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Door';
  ExecuteCommand(G, cmdUse);
  CheckInt('the second ID has to match too', G.World.Counters[1], 0);

  { A person is not a second object - that is what GIVE is for, and saying
    so beats "you don't see that here" when they are standing right there }
  BuildWorld(G);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Merchant';
  ExecuteCommand(G, cmdUse);
  Check('using a thing on a person points at GIVE',
        Pos('giving', G.LastMessage) > 0);
end;

procedure TestGiveTo;
var
  G: TGame;
begin
  WriteLn('etGiveTo:');

  BuildWorld(G);
  AddCountingEvent(G, 1, etGiveTo, 1, 1);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Merchant';
  ExecuteCommand(G, cmdGive);
  CheckInt('firing on object and mob', G.World.Counters[1], 1);

  { Nothing moves by itself: what a gift means is the author's decision, and
    an item silently swallowed by an NPC nobody wrote a response for could
    strand the game }
  CheckInt('the object stays in hand', G.World.PlayerInvCount, 1);

  BuildWorld(G);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Merchant';
  ExecuteCommand(G, cmdGive);
  Check('an unanswered gift is refused, not swallowed',
        Pos('does not want', G.LastMessage) > 0);
  CheckInt('and the object is still in hand', G.World.PlayerInvCount, 1);

  { You cannot give away what you are not holding }
  BuildWorld(G);
  G.World.PlayerInvCount := 0;
  G.World.Objects[1].RoomID := 1;
  AddCountingEvent(G, 1, etGiveTo, 1, 1);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Merchant';
  ExecuteCommand(G, cmdGive);
  CheckInt('giving what you do not carry fires nothing',
           G.World.Counters[1], 0);

  { And there has to be someone to give it to }
  BuildWorld(G);
  AddCountingEvent(G, 1, etGiveTo, 1, 1);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Nobody';
  ExecuteCommand(G, cmdGive);
  CheckInt('giving to no one fires nothing', G.World.Counters[1], 0);
end;

{ The reason PrevNoun2 exists. AGAIN replays PrevCmd with PrevNoun; without
  a second slot beside it, AGAIN after USE KEY ON DOOR replays the bare USE
  KEY - a different command that fires a different trigger. }
procedure TestAgainKeepsBothNouns;
var
  G: TGame;
begin
  WriteLn('AGAIN replays both nouns:');

  BuildWorld(G);
  AddCountingEvent(G, 1, etUseObjectOn, 1, 2);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Door';
  ExecuteCommand(G, cmdUse);
  CheckInt('the first USE fires', G.World.Counters[1], 1);

  { As if the player had typed G. LastNoun is deliberately left holding the
    previous command's noun, which is what RunGame does. }
  ExecuteCommand(G, cmdAgain);
  CheckInt('AGAIN fires it a second time', G.World.Counters[1], 2);
  CheckStr('and the second noun came back', G.LastNoun2, 'Door');

  { A one-noun command afterwards must not leave the old second noun behind }
  BuildWorld(G);
  AddCountingEvent(G, 1, etUseObjectOn, 1, 2);
  G.LastNoun := 'Key';
  G.LastNoun2 := 'Door';
  ExecuteCommand(G, cmdUse);
  G.LastNoun := 'Key';
  G.LastNoun2 := '';
  ExecuteCommand(G, cmdUse);
  ExecuteCommand(G, cmdAgain);
  CheckInt('AGAIN after a bare USE stays bare', G.World.Counters[1], 1);
end;

begin
  Failures := 0;
  WriteLn('parsetest - two-noun commands');
  WriteLn;

  TestPrepositions;
  WriteLn;
  TestOnlyTwoVerbsSplit;
  WriteLn;
  TestNoun2IsCleared;
  WriteLn;
  TestUseObjectOn;
  WriteLn;
  TestGiveTo;
  WriteLn;
  TestAgainKeepsBothNouns;
  WriteLn;

  if Failures = 0 then
    WriteLn('All checks passed.')
  else
  begin
    WriteLn(Failures, ' check(s) FAILED.');
    Halt(1);
  end;
end.
