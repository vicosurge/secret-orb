{ eventtest.pas - Unit tests for the event system.

  Run by "make test" and by the DOS test script. A plain WriteLn program on
  purpose: DOS output redirection can capture this, which is how CI checks it
  under FreeDOS. The game and the editor use Crt and cannot be driven that way.

  Covers the parts of the event system with no other regression cover: the
  binary v4 round-trip, save v3 round-trip, and backward compatibility with
  worlds and saves written before events existed.

  Its scratch files are 8.3 names on purpose. They are written into the
  working directory and deleted again, and the working directory under the
  DOS test run is a FAT volume whose kernel need not support long names. }
program EventTest;

{$MODE OBJFPC}

uses
  SysUtils, GameData, DataFile, Events, WorldVal;

var
  Failures: Integer = 0;
  Checks: Integer = 0;

procedure Check(const What: string; Cond: Boolean);
begin
  Inc(Checks);
  if Cond then
    WriteLn('  ok    ', What)
  else
  begin
    WriteLn('  FAIL  ', What);
    Inc(Failures);
  end;
end;

procedure CheckInt(const What: string; Got, Want: LongInt);
begin
  Check(What + ' = ' + IntToStr(Want) + ' (got ' + IntToStr(Got) + ')',
        Got = Want);
end;

procedure Heading(const S: string);
begin
  WriteLn;
  WriteLn(S);
end;

{ A minimal world that is valid enough to save and load: two rooms, one
  object, one mob. Events are added on top by each test. }
procedure BuildWorld(var W: TGameWorld);
begin
  InitWorld(W);
  W.Title := 'Event Test World';
  W.CurrentRoom := 1;

  W.RoomCount := 2;
  W.Rooms[1].ID := 1;
  W.Rooms[1].Name := 'First Room';
  W.Rooms[1].Desc := 'The first room.';
  W.Rooms[1].Exits[dirNorth] := 2;
  W.Rooms[1].Active := True;
  W.Rooms[2].ID := 2;
  W.Rooms[2].Name := 'Second Room';
  W.Rooms[2].Desc := 'The second room.';
  W.Rooms[2].Exits[dirSouth] := 1;
  W.Rooms[2].Active := True;

  W.ObjectCount := 1;
  W.Objects[1].ID := 1;
  W.Objects[1].Name := 'Lamp';
  W.Objects[1].Desc := 'A brass lamp.';
  W.Objects[1].RoomID := 1;
  W.Objects[1].Flags := [ofPickup];
  W.Objects[1].Active := True;

  W.MobCount := 1;
  W.Mobs[1].ID := 1;
  W.Mobs[1].Name := 'Guard';
  W.Mobs[1].Desc := 'A bored guard.';
  W.Mobs[1].RoomID := 2;
  W.Mobs[1].Dialogue := 'Move along.';
  W.Mobs[1].Active := True;
end;

{ An event exercising every field the binary record carries, so a layout slip
  in any one of them shows up as a mismatch after the round-trip. }
procedure AddFullEvent(var W: TGameWorld);
begin
  { Deliberately sparse: slots 3 and 7, with 1, 2 and 4..6 empty. The slot
    number is the event's identity, so the gaps have to survive a round trip. }
  W.EventCount := 7;

  W.Events[3].Name := 'Fuse burns down';
  W.Events[3].TriggerType := etTimer;
  W.Events[3].TriggerID := 5;
  W.Events[3].TriggerID2 := 0;
  W.Events[3].CondCount := 2;
  W.Events[3].Conditions[1].CondType := ctHasObject;
  W.Events[3].Conditions[1].TargetID := 1;
  W.Events[3].Conditions[1].Negate := False;
  W.Events[3].Conditions[2].CondType := ctCounterGreater;
  W.Events[3].Conditions[2].TargetID := 3;
  W.Events[3].Conditions[2].Value := -7;   { negative, to prove SmallInt }
  W.Events[3].Conditions[2].Negate := True;
  W.Events[3].ActionCount := 3;
  W.Events[3].Actions[1].ActionType := atShowMessage;
  W.Events[3].Actions[1].Text := 'The fuse sputters and goes out.';
  W.Events[3].Actions[2].ActionType := atSetFlag;
  W.Events[3].Actions[2].TargetID := 4;
  W.Events[3].Actions[3].ActionType := atAddCounter;
  W.Events[3].Actions[3].TargetID := 3;
  W.Events[3].Actions[3].Value := -2;
  W.Events[3].OneShot := True;
  W.Events[3].Active := True;

  W.Events[7].Name := 'Guard reacts';
  W.Events[7].TriggerType := etFlagSet;
  W.Events[7].TriggerID := 4;
  W.Events[7].CondCount := 0;
  W.Events[7].ActionCount := 1;
  W.Events[7].Actions[1].ActionType := atShowParagraph;
  W.Events[7].Actions[1].TargetID := 12;
  W.Events[7].OneShot := False;
  W.Events[7].Enabled := False;    { starts disabled - a two-stage puzzle }
  W.Events[7].Active := True;

  W.FlagNames[4] := 'fuse_lit';
  W.CounterNames[3] := 'gunpowder';
end;

procedure TestBinaryRoundTrip;
var
  W, L: TGameWorld;
  Path: string;
begin
  Heading('Binary v4 carries events, flag names and counter names');
  Path := 'EVW.DAT';

  BuildWorld(W);
  AddFullEvent(W);
  Check('world saves', SaveWorld(Path, W));
  Check('world loads', LoadWorld(Path, L));

  CheckInt('highest used slot', L.EventCount, 7);
  Check('empty slots stay empty', not L.Events[1].Active and
        not L.Events[2].Active and not L.Events[5].Active);
  Check('slot 3 is in use', L.Events[3].Active);
  Check('slot 7 is in use', L.Events[7].Active);
  Check('event 1 name survives', L.Events[3].Name = 'Fuse burns down');
  Check('event 1 trigger is etTimer', L.Events[3].TriggerType = etTimer);
  CheckInt('event 1 trigger turn', L.Events[3].TriggerID, 5);
  CheckInt('event 1 condition count', L.Events[3].CondCount, 2);
  Check('condition 1 type', L.Events[3].Conditions[1].CondType = ctHasObject);
  Check('condition 2 type',
        L.Events[3].Conditions[2].CondType = ctCounterGreater);
  CheckInt('condition 2 negative value',
           L.Events[3].Conditions[2].Value, -7);
  Check('condition 2 negate', L.Events[3].Conditions[2].Negate);
  CheckInt('event 1 action count', L.Events[3].ActionCount, 3);
  Check('action 1 is a message',
        L.Events[3].Actions[1].ActionType = atShowMessage);
  Check('action 1 text survives',
        L.Events[3].Actions[1].Text = 'The fuse sputters and goes out.');
  CheckInt('action 3 negative value', L.Events[3].Actions[3].Value, -2);
  Check('event 1 is one-shot', L.Events[3].OneShot);

  Check('event 2 trigger is etFlagSet', L.Events[7].TriggerType = etFlagSet);
  Check('event 2 is not one-shot', not L.Events[7].OneShot);
  Check('action cites a paragraph',
        L.Events[7].Actions[1].ActionType = atShowParagraph);
  CheckInt('paragraph cited', L.Events[7].Actions[1].TargetID, 12);

  Check('an event authored disabled stays disabled', not L.Events[7].Enabled);
  Check('and that reaches the live bitmap', not L.EvEnabled[7]);
  Check('an event authored enabled stays enabled', L.Events[3].Enabled);
  Check('flag name survives', L.FlagNames[4] = 'fuse_lit');
  Check('counter name survives', L.CounterNames[3] = 'gunpowder');

  DeleteFile(Path);
end;

procedure TestReproducible;
var
  W: TGameWorld;
  A, B: string;
  FA, FB: File;
  BufA, BufB: array[0..8191] of Byte;
  NA, NB: Integer;
begin
  Heading('Saving the same world twice yields identical bytes');
  A := 'EVA.DAT';
  B := 'EVB.DAT';

  BuildWorld(W);
  AddFullEvent(W);
  SaveWorld(A, W);
  SaveWorld(B, W);

  Assign(FA, A); Reset(FA, 1); BlockRead(FA, BufA, SizeOf(BufA), NA); Close(FA);
  Assign(FB, B); Reset(FB, 1); BlockRead(FB, BufB, SizeOf(BufB), NB); Close(FB);

  CheckInt('same length', NA, NB);
  Check('same bytes', (NA = NB) and CompareMem(@BufA, @BufB, NA));

  DeleteFile(A);
  DeleteFile(B);
end;

procedure TestEventFreeWorldIsCheap;
var
  W: TGameWorld;
  Bare, Evt: string;
  F: File;
  SizeBare, SizeEvt: LongInt;
begin
  Heading('A world with no events costs only the three section counts');
  Bare := 'EVBARE.DAT';
  Evt := 'EVEVT.DAT';

  BuildWorld(W);
  SaveWorld(Bare, W);
  Assign(F, Bare); Reset(F, 1); SizeBare := FileSize(F); Close(F);

  AddFullEvent(W);
  SaveWorld(Evt, W);
  Assign(F, Evt); Reset(F, 1); SizeEvt := FileSize(F); Close(F);

  Check('a world with events is larger', SizeEvt > SizeBare);
  { The regression guard for the record encoding. A fixed-length record would
    reserve MAX_ACTIONS x MAX_EVENT_TEXT per event and put these two events at
    about 1550 bytes; the variable-length one writes only the fields in use.
    If this ever fails, something has gone back to padding. }
  Check('two events cost well under 300 bytes, not 1500',
        (SizeEvt - SizeBare) < 300);
  Check('five empty slots between them cost nothing',
        (SizeEvt - SizeBare) < 300);
  WriteLn('  note  no events = ', SizeBare, ' bytes, two events (slots 3 and 7) = ',
          SizeEvt, ' bytes, delta = ', SizeEvt - SizeBare);

  DeleteFile(Bare);
  DeleteFile(Evt);
end;

procedure TestSaveGameRoundTrip;
var
  W, L: TGameWorld;
  Path, WPath: string;
begin
  Heading('Save v3 carries flags, counters and fired events');
  Path := 'eventtest.sav';
  WPath := 'EVS.DAT';

  BuildWorld(W);
  AddFullEvent(W);
  SaveWorld(WPath, W);

  W.Flags[4] := True;
  W.Flags[64] := True;          { the last bit, to catch an off-by-one }
  W.Counters[3] := -1234;
  W.Counters[32] := 999;
  W.Fired[1] := True;
  W.EvEnabled[2] := False;
  W.Turns := 42;
  W.Score := 7;

  Check('save writes', SaveGameState(Path, W));

  { Reload the world from disk, so the restore lands on a world that has never
    seen the runtime state - exactly what happens when a player restores. }
  Check('world reloads', LoadWorld(WPath, L));
  Check('save restores', LoadGameState(Path, L));

  Check('flag 4 restored', L.Flags[4]);
  Check('flag 64 restored', L.Flags[64]);
  Check('flag 1 still clear', not L.Flags[1]);
  CheckInt('counter 3 restored', L.Counters[3], -1234);
  CheckInt('counter 32 restored', L.Counters[32], 999);
  Check('event 1 remembered as fired', L.Fired[1]);
  Check('event 2 not fired', not L.Fired[2]);
  Check('event 2 remembered as disabled', not L.EvEnabled[2]);
  Check('event 1 still enabled', L.EvEnabled[1]);
  CheckInt('turns restored', L.Turns, 42);

  DeleteFile(Path);
  DeleteFile(WPath);
end;

{ The text format is the one an author can hand-edit, so it has to survive a
  full trip as faithfully as the binary one - including a message containing a
  comma, which the ACTION= encoding has to keep out of its field splitting. }
procedure TestTextRoundTrip;
var
  W, L: TGameWorld;
  Path: string;
begin
  Heading('Text format carries events');
  Path := 'EV.TXT';

  BuildWorld(W);
  AddFullEvent(W);
  W.Events[3].Actions[1].Text := 'The fuse sputters, gutters, and goes out.';
  Check('text saves', SaveWorldAs(Path, W, sfText));
  Check('text loads', LoadWorld(Path, L));

  CheckInt('highest used slot', L.EventCount, 7);
  Check('empty slots stay empty', not L.Events[1].Active and
        not L.Events[2].Active and not L.Events[5].Active);
  Check('slot 3 is in use', L.Events[3].Active);
  Check('slot 7 is in use', L.Events[7].Active);
  Check('name survives', L.Events[3].Name = 'Fuse burns down');
  Check('trigger survives', L.Events[3].TriggerType = etTimer);
  CheckInt('trigger turn survives', L.Events[3].TriggerID, 5);
  CheckInt('condition count', L.Events[3].CondCount, 2);
  Check('condition type survives',
        L.Events[3].Conditions[2].CondType = ctCounterGreater);
  CheckInt('negative condition value', L.Events[3].Conditions[2].Value, -7);
  Check('negate survives', L.Events[3].Conditions[2].Negate);
  CheckInt('action count', L.Events[3].ActionCount, 3);
  Check('a message with commas survives intact',
        L.Events[3].Actions[1].Text =
        'The fuse sputters, gutters, and goes out.');
  Check('action order is preserved',
        (L.Events[3].Actions[2].ActionType = atSetFlag) and
        (L.Events[3].Actions[3].ActionType = atAddCounter));
  CheckInt('negative action value', L.Events[3].Actions[3].Value, -2);
  Check('not-one-shot survives', not L.Events[7].OneShot);
  Check('an event authored disabled stays disabled', not L.Events[7].Enabled);
  Check('and that reaches the live bitmap', not L.EvEnabled[7]);
  Check('an event authored enabled stays enabled', L.Events[3].Enabled);
  Check('flag name survives', L.FlagNames[4] = 'fuse_lit');
  Check('counter name survives', L.CounterNames[3] = 'gunpowder');

  DeleteFile(Path);
end;

{ BPL is the third format all three editors read and write, so it gets the
  same trip. A brace in a message would end the tag early, so the writer
  substitutes it - that is checked here rather than left to be discovered. }
procedure TestBPLRoundTrip;
var
  W, L: TGameWorld;
  Path: string;
begin
  Heading('BPL revision 4 carries events');
  Path := 'EV.BPL';

  BuildWorld(W);
  AddFullEvent(W);
  W.Events[3].Actions[1].Text := 'A brace {here} and a comma, too.';
  Check('BPL saves', SaveWorldAs(Path, W, sfBPL));
  Check('BPL loads', LoadWorld(Path, L));

  CheckInt('highest used slot', L.EventCount, 7);
  Check('empty slots stay empty', not L.Events[1].Active and
        not L.Events[2].Active and not L.Events[5].Active);
  Check('slot 3 is in use', L.Events[3].Active);
  Check('slot 7 is in use', L.Events[7].Active);
  Check('name survives', L.Events[3].Name = 'Fuse burns down');
  Check('trigger survives', L.Events[3].TriggerType = etTimer);
  CheckInt('trigger turn survives', L.Events[3].TriggerID, 5);
  CheckInt('condition count', L.Events[3].CondCount, 2);
  Check('condition type survives',
        L.Events[3].Conditions[2].CondType = ctCounterGreater);
  CheckInt('negative condition value', L.Events[3].Conditions[2].Value, -7);
  Check('negate survives', L.Events[3].Conditions[2].Negate);
  CheckInt('action count', L.Events[3].ActionCount, 3);
  Check('braces are substituted, not left to break the tag',
        L.Events[3].Actions[1].Text = 'A brace (here) and a comma, too.');
  Check('action order is preserved',
        (L.Events[3].Actions[2].ActionType = atSetFlag) and
        (L.Events[3].Actions[3].ActionType = atAddCounter));
  Check('not-one-shot survives', not L.Events[7].OneShot);
  Check('paragraph action survives',
        L.Events[7].Actions[1].ActionType = atShowParagraph);
  Check('an event authored disabled stays disabled', not L.Events[7].Enabled);
  Check('and that reaches the live bitmap', not L.EvEnabled[7]);
  Check('an event authored enabled stays enabled', L.Events[3].Enabled);
  Check('flag name survives', L.FlagNames[4] = 'fuse_lit');
  Check('counter name survives', L.CounterNames[3] = 'gunpowder');

  DeleteFile(Path);
end;

procedure TestBackwardCompat;
var
  W, L: TGameWorld;
  Path: string;
begin
  Heading('A world with no events loads with clean event state');
  Path := 'EVOLD.DAT';

  BuildWorld(W);
  SaveWorld(Path, W);
  Check('loads', LoadWorld(Path, L));
  CheckInt('no events', L.EventCount, 0);
  Check('flags start clear', not L.Flags[1]);
  Check('counters start zero', L.Counters[1] = 0);
  Check('nothing has fired', not L.Fired[1]);
  Check('events start enabled', L.EvEnabled[1]);

  DeleteFile(Path);
end;


{ ---------------------------------------------------------------- engine -- }

{ A bare world with one event in the given slot, so each test starts clean }
procedure OneEvent(var W: TGameWorld; Slot: Word; Trig: TEventTrigger;
                   ID1: Word);
begin
  BuildWorld(W);
  InitEvent(W.Events[Slot]);
  W.Events[Slot].TriggerType := Trig;
  W.Events[Slot].TriggerID := ID1;
  W.Events[Slot].Active := True;
  if Slot > W.EventCount then W.EventCount := Slot;
  SeedEventState(W);
end;

procedure TestConditions;
var
  W: TGameWorld;
  C: TCondition;
begin
  Heading('Conditions read the world');
  BuildWorld(W);

  C.CondType := ctHasObject; C.TargetID := 1; C.Value := 0; C.Negate := False;
  Check('ctHasObject is false when the object is on the floor',
        not EvalCondition(W, C));
  W.PlayerInvCount := 1;
  W.PlayerInventory[1] := 1;
  Check('ctHasObject is true once carried', EvalCondition(W, C));
  C.Negate := True;
  Check('Negate inverts it', not EvalCondition(W, C));
  C.Negate := False;

  C.CondType := ctFlagIsSet; C.TargetID := 5;
  Check('ctFlagIsSet is false while clear', not EvalCondition(W, C));
  W.Flags[5] := True;
  Check('ctFlagIsSet is true once set', EvalCondition(W, C));
  C.CondType := ctFlagIsClear;
  Check('ctFlagIsClear is its opposite', not EvalCondition(W, C));

  C.CondType := ctCounterGreater; C.TargetID := 2; C.Value := 3;
  W.Counters[2] := 3;
  Check('ctCounterGreater is strict', not EvalCondition(W, C));
  W.Counters[2] := 4;
  Check('ctCounterGreater at 4 > 3', EvalCondition(W, C));
  C.CondType := ctCounterLess; C.Value := -2; W.Counters[2] := -5;
  Check('ctCounterLess handles negatives', EvalCondition(W, C));

  C.CondType := ctRoomIs; C.TargetID := 1; C.Value := 0;
  Check('ctRoomIs matches the current room', EvalCondition(W, C));
  C.TargetID := 2;
  Check('ctRoomIs rejects another room', not EvalCondition(W, C));

  C.CondType := ctVisitedRoom; C.TargetID := 2;
  Check('ctVisitedRoom is false before the visit', not EvalCondition(W, C));
  W.Visited[2] := True;
  Check('ctVisitedRoom is true after it', EvalCondition(W, C));

  { An out-of-range slot must not read past the array }
  C.CondType := ctFlagIsSet; C.TargetID := 9999;
  Check('an out-of-range flag is simply false', not EvalCondition(W, C));
end;

procedure TestConditionsAreAnded;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('All of an event''s conditions must hold');
  OneEvent(W, 1, etTimer, 1);
  W.Events[1].CondCount := 2;
  W.Events[1].Conditions[1].CondType := ctFlagIsSet;
  W.Events[1].Conditions[1].TargetID := 1;
  W.Events[1].Conditions[2].CondType := ctFlagIsSet;
  W.Events[1].Conditions[2].TargetID := 2;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atShowMessage;
  W.Events[1].Actions[1].Text := 'both';
  W.Turns := 1;

  W.Flags[1] := True;
  ClearOutcome(O);
  FireEvents(W, etTimer, 0, 0, O);
  Check('one of two conditions is not enough', O.Message = '');

  W.Flags[2] := True;
  ClearOutcome(O);
  FireEvents(W, etTimer, 0, 0, O);
  Check('both conditions fire it', O.Message = 'both');
end;

procedure TestActions;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('Actions change the world');
  OneEvent(W, 1, etTakeObject, 1);
  W.Events[1].ActionCount := 8;
  W.Events[1].Actions[1].ActionType := atSetFlag;
  W.Events[1].Actions[1].TargetID := 3;
  W.Events[1].Actions[2].ActionType := atSetCounter;
  W.Events[1].Actions[2].TargetID := 1;
  W.Events[1].Actions[2].Value := 10;
  W.Events[1].Actions[3].ActionType := atAddCounter;
  W.Events[1].Actions[3].TargetID := 1;
  W.Events[1].Actions[3].Value := -3;
  W.Events[1].Actions[4].ActionType := atMoveMob;
  W.Events[1].Actions[4].TargetID := 1;
  W.Events[1].Actions[4].Value := 1;
  W.Events[1].Actions[5].ActionType := atAddScore;
  W.Events[1].Actions[5].TargetID := 25;
  W.Events[1].Actions[6].ActionType := atShowParagraph;
  W.Events[1].Actions[6].TargetID := 9;
  W.Events[1].Actions[7].ActionType := atLockExit;
  W.Events[1].Actions[7].TargetID := 1;
  W.Events[1].Actions[7].Value := EncodeExitValue(dirNorth, 0);
  W.Events[1].Actions[8].ActionType := atTeleportPlayer;
  W.Events[1].Actions[8].TargetID := 2;

  ClearOutcome(O);
  FireEvents(W, etTakeObject, 1, 0, O);

  Check('atSetFlag sets it', W.Flags[3]);
  CheckInt('atSetCounter then atAddCounter', W.Counters[1], 7);
  CheckInt('atMoveMob moves the mob', W.Mobs[1].RoomID, 1);
  CheckInt('atAddScore is reported, not applied directly', O.Points, 25);
  CheckInt('atShowParagraph queues one', O.ParaCount, 1);
  CheckInt('and it is the right number', O.Paras[1], 9);
  CheckInt('atLockExit closes the exit', W.Rooms[1].Exits[dirNorth], 0);
  CheckInt('atTeleportPlayer is reported, not applied', O.Teleport, 2);
  CheckInt('and the room is unchanged until gamecore acts', W.CurrentRoom, 1);

  { Unlock has to name the destination, because locking discarded it }
  ClearOutcome(O);
  InitEvent(W.Events[1]);
  W.Events[1].TriggerType := etUseObject;
  W.Events[1].Active := True;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atUnlockExit;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := EncodeExitValue(dirNorth, 2);
  SeedEventState(W);
  FireEvents(W, etUseObject, 1, 0, O);
  CheckInt('atUnlockExit restores it', W.Rooms[1].Exits[dirNorth], 2);
end;

procedure TestRemovalKeepsActive;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('Removing an object moves it out of play, never deactivates it');
  OneEvent(W, 1, etUseObject, 1);
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atRemoveObject;
  W.Events[1].Actions[1].TargetID := 1;

  ClearOutcome(O);
  FireEvents(W, etUseObject, 1, 0, O);
  CheckInt('the object is nowhere', W.Objects[1].RoomID, 0);
  { The save writes one record per active object and validates the body
    length against that count, so clearing Active would make every existing
    save read as truncated }
  Check('but it is still Active, or saves would break',
        W.Objects[1].Active);
end;

procedure TestOneShotAndEnable;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('One-shot and enable/disable');
  OneEvent(W, 1, etUseObject, 1);
  W.Events[1].OneShot := True;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atAddCounter;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := 1;

  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  CheckInt('a one-shot event fires exactly once', W.Counters[1], 1);

  W.Events[1].OneShot := False;
  SeedEventState(W);
  W.Counters[1] := 0;
  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  CheckInt('a repeating event fires every time', W.Counters[1], 2);

  W.EvEnabled[1] := False;
  ClearOutcome(O); FireEvents(W, etUseObject, 1, 0, O);
  CheckInt('a disabled event fires not at all', W.Counters[1], 2);
end;

procedure TestTimer;
var
  W: TGameWorld;
  O: TEventOutcome;
  T: Integer;
begin
  Heading('Timers fire on the turn counter');
  OneEvent(W, 1, etTimer, 5);
  W.Events[1].OneShot := True;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atAddCounter;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := 1;

  for T := 1 to 10 do
  begin
    W.Turns := T;
    ClearOutcome(O);
    FireEvents(W, etTimer, 0, 0, O);
  end;
  CheckInt('a one-shot timer fires once, at its turn', W.Counters[1], 1);

  { Periodic: from turn 4, every 3 turns - 4, 7, 10 }
  OneEvent(W, 1, etTimer, 4);
  W.Events[1].TriggerID2 := 3;
  W.Events[1].OneShot := False;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atAddCounter;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := 1;
  for T := 1 to 10 do
  begin
    W.Turns := T;
    ClearOutcome(O);
    FireEvents(W, etTimer, 0, 0, O);
  end;
  CheckInt('a periodic timer fires on 4, 7 and 10', W.Counters[1], 3);
end;

procedure TestCascadeTerminates;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('A cascade is bounded and cannot hang the game');

  { Edge triggering alone kills the common case: an event that re-sets a flag
    that is already set fires nothing. }
  OneEvent(W, 1, etFlagSet, 1);
  W.Events[1].OneShot := False;
  W.Events[1].ActionCount := 2;
  W.Events[1].Actions[1].ActionType := atSetFlag;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[2].ActionType := atAddCounter;
  W.Events[1].Actions[2].TargetID := 1;
  W.Events[1].Actions[2].Value := 1;

  ClearOutcome(O);
  W.Flags[1] := True;
  FireEvents(W, etFlagSet, 1, 0, O);
  CheckInt('re-setting a set flag fires nothing', W.Counters[1], 1);

  { A deliberate toggle loop: two events each toggling the flag the other
    watches. This is the case only the depth and budget bounds can stop. }
  BuildWorld(W);
  InitEvent(W.Events[1]);
  W.Events[1].TriggerType := etFlagSet;
  W.Events[1].TriggerID := 1;
  W.Events[1].OneShot := False;
  W.Events[1].ActionCount := 2;
  W.Events[1].Actions[1].ActionType := atClearFlag;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[2].ActionType := atAddCounter;
  W.Events[1].Actions[2].TargetID := 1;
  W.Events[1].Actions[2].Value := 1;
  W.Events[1].Active := True;

  InitEvent(W.Events[2]);
  W.Events[2].TriggerType := etFlagClear;
  W.Events[2].TriggerID := 1;
  W.Events[2].OneShot := False;
  W.Events[2].ActionCount := 1;
  W.Events[2].Actions[1].ActionType := atSetFlag;
  W.Events[2].Actions[1].TargetID := 1;
  W.Events[2].Active := True;
  W.EventCount := 2;
  SeedEventState(W);

  ClearOutcome(O);
  { Flag 1 starts set, so event 1's atClearFlag is a genuine transition and
    the cascade really runs - with it clear, edge triggering would short the
    whole thing out and the test would prove nothing. }
  W.Flags[1] := True;
  { If this does not terminate, the test hangs rather than fails - which is
    itself the signal, and is why the bound is not left to chance. }
  FireEvents(W, etFlagSet, 1, 0, O);
  Check('a mutually-toggling pair terminates', True);
  Check('the cascade really ran', W.Counters[1] > 1);
  Check('and it stopped well short of a runaway', W.Counters[1] <= 8);
  WriteLn('  note  the toggle loop ran ', W.Counters[1], ' times before the bound');
end;

procedure TestAllMatchingEventsFire;
var
  W: TGameWorld;
  O: TEventOutcome;
begin
  Heading('Every matching event fires, in slot order');
  BuildWorld(W);
  InitEvent(W.Events[2]);
  W.Events[2].TriggerType := etUseObject;
  W.Events[2].TriggerID := 1;
  W.Events[2].ActionCount := 1;
  W.Events[2].Actions[1].ActionType := atShowMessage;
  W.Events[2].Actions[1].Text := 'first';
  W.Events[2].Active := True;

  InitEvent(W.Events[5]);
  W.Events[5].TriggerType := etUseObject;
  W.Events[5].TriggerID := 0;        { 0 means any object }
  W.Events[5].ActionCount := 1;
  W.Events[5].Actions[1].ActionType := atShowMessage;
  W.Events[5].Actions[1].Text := 'second';
  W.Events[5].Active := True;
  W.EventCount := 5;
  SeedEventState(W);

  ClearOutcome(O);
  FireEvents(W, etUseObject, 1, 0, O);
  Check('both fired, low slot first', O.Message = 'first second');
end;

{ ---- Validation ---------------------------------------------------------

  Everything an event names is a number, and a wrong number is invisible at
  run time - the interpreter skips what it cannot resolve without a word.
  These tests are the reason worldval.pas checks events at all, so each one
  names the mistake it is standing in for. }

{ Runs the validator and says whether any issue mentions Frag }
function Reports(var W: TGameWorld; const Frag: string): Boolean;
var
  List: TIssueList;
  N, I: Integer;
begin
  Result := False;
  N := ValidateWorld(W, List);
  for I := 1 to N do
    if Pos(Frag, List[I].Text) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function ErrorCount(var W: TGameWorld): Integer;
var
  List: TIssueList;
  N, I: Integer;
begin
  Result := 0;
  N := ValidateWorld(W, List);
  for I := 1 to N do
    if List[I].Level = ilError then Inc(Result);
end;

{ A world with one event in slot 1, named and enabled, with room 1 as its
  trigger and one harmless action. Tests spoil one field at a time. }
procedure BuildEventWorld(var W: TGameWorld);
begin
  BuildWorld(W);
  W.WinRoomID := 2;                 { or every check trips the no-win warning }
  InitEvent(W.Events[1]);
  W.Events[1].Name := 'Arrival';
  W.Events[1].TriggerType := etEnterRoom;
  W.Events[1].TriggerID := 1;
  W.Events[1].ActionCount := 1;
  W.Events[1].Actions[1].ActionType := atShowMessage;
  W.Events[1].Actions[1].Text := 'Something stirs.';
  W.Events[1].Active := True;
  W.EventCount := 1;
  SeedEventState(W);
end;

procedure TestValidation;
var
  W: TGameWorld;
begin
  Heading('Validation catches what the interpreter passes over in silence');

  BuildEventWorld(W);
  CheckInt('a sound event reports no errors', ErrorCount(W), 0);

  { A trigger on a room that was deleted afterwards }
  BuildEventWorld(W);
  W.Events[1].TriggerID := 99;
  Check('a trigger naming a missing room', Reports(W, 'names room 99'));

  { etEnterRoom's hook passes 0 as the second ID, so a TriggerID2 set here
    can never match and the event is dead rather than merely narrow }
  BuildEventWorld(W);
  W.Events[1].TriggerID2 := 2;
  Check('a second ID on a trigger that has none',
        Reports(W, 'never fires'));

  { The timer hook runs after the turn counter is incremented, so turn 0
    never arrives; with no repeat period either, the event waits forever }
  BuildEventWorld(W);
  W.Events[1].TriggerType := etTimer;
  W.Events[1].TriggerID := 0;
  W.Events[1].TriggerID2 := 0;
  Check('a timer set to turn 0 with no period',
        Reports(W, 'never fires'));

  { The mistake CheckPara exists for, now reachable from an action }
  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atShowParagraph;
  W.Events[1].Actions[1].TargetID := 5;
  Check('an action naming an empty paragraph', Reports(W, 'which is empty'));

  BuildEventWorld(W);
  W.Events[1].Actions[1].Text := '';
  Check('an action showing an empty message',
        Reports(W, 'empty message'));

  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atSetFlag;
  W.Events[1].Actions[1].TargetID := MAX_FLAGS + 1;
  Check('a flag number past the end', Reports(W, 'flags run 1 to'));

  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atAddCounter;
  W.Events[1].Actions[1].TargetID := 0;
  Check('counter 0, which does not exist', Reports(W, 'counters run 1 to'));

  { Slot numbers are identity, so naming an empty one is a live risk }
  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atEnableEvent;
  W.Events[1].Actions[1].TargetID := 9;
  Check('an action enabling an empty slot', Reports(W, 'empty slot'));

  { The direction rides in the low three bits of Value, which can hold 6 and
    7. DecodeExitValue clamps those to north rather than crashing, so the
    author gets the wrong door locked and no message. }
  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atLockExit;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := 6;
  Check('an exit action with no such direction',
        Reports(W, 'not a direction'));

  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atUnlockExit;
  W.Events[1].Actions[1].TargetID := 1;
  W.Events[1].Actions[1].Value := EncodeExitValue(dirNorth, 99);
  Check('an unlock onto a room that does not exist',
        Reports(W, 'names room 99'));

  { FireEvents walks slots 1..EventCount and stops }
  BuildEventWorld(W);
  W.Events[4] := W.Events[1];
  Check('an active event above the event count',
        Reports(W, 'above the event count'));

  BuildEventWorld(W);
  W.Events[1].Enabled := False;
  SeedEventState(W);
  Check('an event that starts disabled with nothing to enable it',
        Reports(W, 'starts disabled'));

  { The event equivalent of an unreachable room }
  BuildEventWorld(W);
  W.Events[1].TriggerType := etFlagSet;
  W.Events[1].TriggerID := 3;
  Check('a flag trigger no action ever writes',
        Reports(W, 'no action ever writes'));

  { ...and the same event once something does set the flag }
  W.Events[2] := W.Events[1];
  W.Events[2].TriggerType := etEnterRoom;
  W.Events[2].TriggerID := 1;
  W.Events[2].Actions[1].ActionType := atSetFlag;
  W.Events[2].Actions[1].TargetID := 3;
  W.EventCount := 2;
  SeedEventState(W);
  Check('and stays quiet once one does',
        not Reports(W, 'no action ever writes'));
end;

{ An atShowParagraph action is the seventh way to reach a paragraph and the
  only one that is not a field on an entity. If the cross-reference does not
  know about it, every event-fired paragraph is reported as an orphan - and
  "fired by: NOTHING" is the one line in that file an author acts on. }
{ Reads the file back looking for one line containing Frag. Line by line
  rather than into one buffer: these units compile with short strings on, so
  a whole file accumulated into a "string" would be cut at 255 characters. }
function FileHasLine(const Path, Frag: string): Boolean;
var
  F: Text;
  Line: string;
begin
  Result := False;
  Assign(F, Path);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit;
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    if Pos(Frag, Line) > 0 then Result := True;
  end;
  Close(F);
end;

procedure TestParaXRefKnowsEvents;
var
  W: TGameWorld;
  Path: string;
begin
  Heading('The paragraph cross-reference names event actions');

  Path := 'EVX.TXT';
  BuildEventWorld(W);
  SetParagraph(W, 3, 'The floor gives way beneath you.');
  W.ParaCount := 3;
  W.Events[1].Actions[1].ActionType := atShowParagraph;
  W.Events[1].Actions[1].TargetID := 3;
  W.Events[1].Actions[1].Text := '';

  Check('the cross-reference is written', WriteParaXRef(Path, W));
  Check('paragraph 3 is credited to the event',
        FileHasLine(Path, 'fired by: event 1'));
  Check('and is not called an orphan',
        not FileHasLine(Path, 'NOTHING'));

  { The other half: an action naming a slot with no text says nothing at all
    at run time, so this file is where the author finds it }
  BuildEventWorld(W);
  W.Events[1].Actions[1].ActionType := atShowParagraph;
  W.Events[1].Actions[1].TargetID := 9;
  W.Events[1].Actions[1].Text := '';
  Check('the cross-reference is rewritten', WriteParaXRef(Path, W));
  Check('an action naming an empty slot is listed',
        FileHasLine(Path, 'Event 1 action 1 names paragraph 9'));

  DeleteFile(Path);
end;

begin
  WriteLn('Secret Orb event system tests');
  WriteLn('=============================');

  TestBinaryRoundTrip;
  TestReproducible;
  TestEventFreeWorldIsCheap;
  TestSaveGameRoundTrip;
  TestTextRoundTrip;
  TestBPLRoundTrip;
  TestBackwardCompat;

  TestConditions;
  TestConditionsAreAnded;
  TestActions;
  TestRemovalKeepsActive;
  TestOneShotAndEnable;
  TestTimer;
  TestCascadeTerminates;
  TestAllMatchingEventsFire;

  TestValidation;
  TestParaXRefKnowsEvents;

  WriteLn;
  if Failures = 0 then
  begin
    WriteLn('Events: all ', Checks, ' checks passed');
    Halt(0);
  end
  else
  begin
    WriteLn('Events: ', Failures, ' of ', Checks, ' checks FAILED');
    Halt(1);
  end;
end.
