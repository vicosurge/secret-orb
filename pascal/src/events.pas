{ events.pas - The event interpreter for Secret Orb.

  Triggers, conditions and actions, as authored in the world file. This is
  engine code and ships in the game; the checks that tell an author their
  event is broken live in worldval.pas, which does not.

  It uses GameData and nothing else. In particular it never touches Crt or
  Display, and it must never use GameCore - TGame lives there, so a call in
  that direction would be a unit cycle. Anything the player should see comes
  back in a TEventOutcome, which gamecore.pas drains at a scene boundary. Two
  things fall out of that: the interpreter can be driven from a plain WriteLn
  test program (tools/eventtest.pas, which CI runs on Linux and under
  FreeDOS), and a paragraph fired by an event goes through exactly the same
  ShowParagraph as the six built-in story triggers, so booklet mode and the
  paragraph cross-reference keep working with no special case. }
unit Events;

{$MODE OBJFPC}

interface

uses
  SysUtils, GameData;

const
  MAX_PARA_QUEUE = 8;       { Paragraphs one batch of events may queue }
  MAX_EVENT_DEPTH = 4;      { Flag-set -> event -> flag-set recursion bound }
  MAX_TURN_ACTIONS = 256;   { Total actions one triggering may execute }

type
  TEndKind = (ekNone, ekWin, ekLose);

  TEventOutcome = record
    Message: string;                          { Appended to LastMessage }
    Paras: array[1..MAX_PARA_QUEUE] of Word;  { In the order they fired }
    ParaCount: Byte;
    Points: Word;                             { Score awarded this batch }
    Ending: TEndKind;
    Teleport: Word;                           { Room to move to, 0 = stay }
  end;

procedure ClearOutcome(var O: TEventOutcome);

{ True when an event left anything for gamecore to do. Asked by ApplyOutcome
  before it drains, and by HandleGive to find out whether the world had an
  answer of its own before it offers a default one. }
function HasOutcome(const O: TEventOutcome): Boolean;

{ Fires every event matching the trigger, in ascending slot order. All of them
  fire, not just the first - which is how an author writes more than
  MAX_ACTIONS actions for one trigger: two events with the same trigger. }
procedure FireEvents(var W: TGameWorld; Trig: TEventTrigger;
                     ID1, ID2: Word; var O: TEventOutcome);

{ Exposed for tools/eventtest.pas and for worldval.pas's dead-condition check }
function EvalCondition(var W: TGameWorld; const C: TCondition): Boolean;

implementation

var
  { Depth and budget are unit-level rather than parameters so that a nested
    fire cannot reset its own bound. Held here rather than on the stack for
    the same reason worldval.pas keeps its scratch arrays there: DOS stacks
    are small. }
  Depth: Integer = 0;
  Budget: Integer = 0;

procedure ClearOutcome(var O: TEventOutcome);
begin
  O.Message := '';
  O.ParaCount := 0;
  O.Points := 0;
  O.Ending := ekNone;
  O.Teleport := 0;
end;

function HasOutcome(const O: TEventOutcome): Boolean;
begin
  Result := (O.Message <> '') or (O.ParaCount > 0) or (O.Points > 0) or
            (O.Ending <> ekNone) or (O.Teleport <> 0);
end;

procedure QueuePara(var O: TEventOutcome; Num: Word);
begin
  if Num = 0 then Exit;
  if O.ParaCount >= MAX_PARA_QUEUE then Exit;
  Inc(O.ParaCount);
  O.Paras[O.ParaCount] := Num;
end;

procedure AddMessage(var O: TEventOutcome; const S: string);
begin
  if S = '' then Exit;
  if O.Message = '' then
    O.Message := S
  else
    O.Message := O.Message + ' ' + S;
end;

{ FindRoomByID lives in datafile.pas, which this unit deliberately does not
  use - the interpreter depends on GameData alone. Rooms are indexed by array
  position everywhere in the engine, so this is the same lookup. }
function RoomIndex(var W: TGameWorld; ID: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  if ID = 0 then Exit;
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active and (W.Rooms[I].ID = ID) then
    begin
      Result := I;
      Exit;
    end;
end;

function FlagOK(N: Word): Boolean;
begin
  Result := (N >= 1) and (N <= MAX_FLAGS);
end;

function CounterOK(N: Word): Boolean;
begin
  Result := (N >= 1) and (N <= MAX_COUNTERS);
end;

function EvalCondition(var W: TGameWorld; const C: TCondition): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  case C.CondType of
    ctNone:
      Result := True;                { An unused slot never blocks anything }
    ctHasObject:
      Result := PlayerHasObject(W, C.TargetID);
    ctObjectInRoom:
      begin
        Idx := FindObjectByID(W, C.TargetID);
        Result := (Idx > 0) and (W.Objects[Idx].CarriedBy = 0) and
                  (W.Objects[Idx].RoomID = Word(C.Value));
      end;
    ctMobInRoom:
      begin
        Idx := FindMobByID(W, C.TargetID);
        Result := (Idx > 0) and (W.Mobs[Idx].RoomID = Word(C.Value));
      end;
    ctFlagIsSet:
      Result := FlagOK(C.TargetID) and W.Flags[C.TargetID];
    ctFlagIsClear:
      Result := FlagOK(C.TargetID) and not W.Flags[C.TargetID];
    ctCounterEquals:
      Result := CounterOK(C.TargetID) and (W.Counters[C.TargetID] = C.Value);
    ctCounterGreater:
      Result := CounterOK(C.TargetID) and (W.Counters[C.TargetID] > C.Value);
    ctCounterLess:
      Result := CounterOK(C.TargetID) and (W.Counters[C.TargetID] < C.Value);
    ctVisitedRoom:
      begin
        Idx := RoomIndex(W, C.TargetID);
        Result := (Idx > 0) and W.Visited[Idx];
      end;
    ctRoomIs:
      Result := W.CurrentRoom = C.TargetID;
  end;

  if C.Negate then Result := not Result;
end;

function ConditionsHold(var W: TGameWorld; const E: TWorldEvent): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to E.CondCount do
    if not EvalCondition(W, E.Conditions[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

{ Forward: the flag actions re-enter the trigger loop }
procedure RunActions(var W: TGameWorld; var E: TWorldEvent;
                     var O: TEventOutcome); forward;

{ Flag writes are edge-triggered: etFlagSet fires only on a genuine False to
  True transition. That alone stops the common runaway, where an event sets a
  flag that is already set and fires itself forever. atToggleFlag can still
  oscillate, which is what the depth and budget bounds are for. }
procedure WriteFlag(var W: TGameWorld; N: Word; NewVal: Boolean;
                    var O: TEventOutcome);
begin
  if not FlagOK(N) then Exit;
  if W.Flags[N] = NewVal then Exit;
  W.Flags[N] := NewVal;
  if NewVal then
    FireEvents(W, etFlagSet, N, 0, O)
  else
    FireEvents(W, etFlagClear, N, 0, O);
end;

procedure SetCounter(var W: TGameWorld; N: Word; V: LongInt);
begin
  if not CounterOK(N) then Exit;
  { Clamped rather than wrapped: an author who adds to a counter every turn
    should get a stuck maximum, not a sudden negative }
  if V < -32768 then V := -32768;
  if V > 32767 then V := 32767;
  W.Counters[N] := V;
end;

procedure RunActions(var W: TGameWorld; var E: TWorldEvent;
                     var O: TEventOutcome);
var
  I, Idx: Integer;
  Dir: TDirection;
  Dest: Word;
begin
  for I := 1 to E.ActionCount do
  begin
    if Budget <= 0 then Exit;
    Dec(Budget);

    with E.Actions[I] do
      case ActionType of
        atNone: ;
        atShowMessage:
          AddMessage(O, Text);
        atShowParagraph:
          QueuePara(O, TargetID);

        atSetFlag:    WriteFlag(W, TargetID, True, O);
        atClearFlag:  WriteFlag(W, TargetID, False, O);
        atToggleFlag:
          if FlagOK(TargetID) then
            WriteFlag(W, TargetID, not W.Flags[TargetID], O);

        atSetCounter: SetCounter(W, TargetID, Value);
        atAddCounter:
          if CounterOK(TargetID) then
            SetCounter(W, TargetID, LongInt(W.Counters[TargetID]) + Value);
        atSubCounter:
          if CounterOK(TargetID) then
            SetCounter(W, TargetID, LongInt(W.Counters[TargetID]) - Value);

        { Value > 0 is a room, 0 is the player's hands. Removal moves the
          object out of play and never clears Active: the save file writes one
          state record per active object and checks the body length against
          the active count, so deactivating one would make every existing save
          read as truncated. }
        atMoveObject, atSpawnObject:
          begin
            Idx := FindObjectByID(W, TargetID);
            if Idx > 0 then
            begin
              W.Objects[Idx].CarriedBy := 0;
              W.Objects[Idx].RoomID := Word(Value);
            end;
          end;
        atRemoveObject:
          begin
            Idx := FindObjectByID(W, TargetID);
            if Idx > 0 then
            begin
              W.Objects[Idx].RoomID := 0;
              W.Objects[Idx].CarriedBy := 0;
            end;
          end;
        atMoveMob:
          begin
            Idx := FindMobByID(W, TargetID);
            if Idx > 0 then W.Mobs[Idx].RoomID := Word(Value);
          end;
        atRemoveMob:
          begin
            Idx := FindMobByID(W, TargetID);
            if Idx > 0 then W.Mobs[Idx].RoomID := 0;
          end;

        atLockExit:
          begin
            DecodeExitValue(Value, Dir, Dest);
            Idx := RoomIndex(W, TargetID);
            if Idx > 0 then W.Rooms[Idx].Exits[Dir] := DIR_NONE;
          end;
        atUnlockExit:
          begin
            DecodeExitValue(Value, Dir, Dest);
            Idx := RoomIndex(W, TargetID);
            if Idx > 0 then W.Rooms[Idx].Exits[Dir] := Dest;
          end;

        { Recorded rather than applied: the room change has to run through the
          arrival machinery in gamecore, which knows about first-visit scoring
          and paragraphs. Applying CurrentRoom here would skip all of it. }
        atTeleportPlayer:
          O.Teleport := TargetID;
        atAddScore:
          Inc(O.Points, TargetID);
        atEndGame:
          if Value = 0 then O.Ending := ekWin else O.Ending := ekLose;

        atEnableEvent:
          if (TargetID >= 1) and (TargetID <= MAX_EVENTS) then
            W.EvEnabled[TargetID] := True;
        atDisableEvent:
          if (TargetID >= 1) and (TargetID <= MAX_EVENTS) then
            W.EvEnabled[TargetID] := False;
      end;
  end;
end;

{ Does this event's trigger match what just happened? }
function TriggerMatches(var W: TGameWorld; const E: TWorldEvent;
                        Trig: TEventTrigger; ID1, ID2: Word): Boolean;
var
  Elapsed: LongInt;
begin
  Result := False;
  if E.TriggerType <> Trig then Exit;

  if Trig = etTimer then
  begin
    { TriggerID is the turn to fire at; TriggerID2 is a repeat period, 0 for
      once only. Because the hook sits inside the branch that increments the
      turn counter, meta commands - HELP, SCORE, SAVE, LOAD, EXITS - do not
      advance a timer, which is the whole reason for hooking there. }
    if W.Turns < E.TriggerID then Exit;
    if W.Turns = E.TriggerID then
      Result := True
    else if E.TriggerID2 > 0 then
    begin
      Elapsed := LongInt(W.Turns) - LongInt(E.TriggerID);
      Result := (Elapsed mod E.TriggerID2) = 0;
    end;
    Exit;
  end;

  { 0 means "any", so an author can write one event for every object }
  if (E.TriggerID <> 0) and (E.TriggerID <> ID1) then Exit;
  if (E.TriggerID2 <> 0) and (E.TriggerID2 <> ID2) then Exit;
  Result := True;
end;

procedure FireEvents(var W: TGameWorld; Trig: TEventTrigger;
                     ID1, ID2: Word; var O: TEventOutcome);
var
  I, Hi: Integer;
  Outermost: Boolean;
begin
  { A world with no events pays one compare per hook }
  if W.EventCount = 0 then Exit;
  if Depth >= MAX_EVENT_DEPTH then Exit;

  { The outermost call owns the budget. A cascade shares it, so a wide
    non-recursive fan-out is bounded too, not just a deep one. }
  Outermost := Depth = 0;
  if Outermost then Budget := MAX_TURN_ACTIONS;

  Inc(Depth);
  Hi := W.EventCount;
  if Hi > MAX_EVENTS then Hi := MAX_EVENTS;

  for I := 1 to Hi do
  begin
    if Budget <= 0 then Break;
    if not W.Events[I].Active then Continue;
    if not W.EvEnabled[I] then Continue;
    if W.Events[I].OneShot and W.Fired[I] then Continue;
    if not TriggerMatches(W, W.Events[I], Trig, ID1, ID2) then Continue;
    if not ConditionsHold(W, W.Events[I]) then Continue;

    { Marked before the actions run, so a one-shot cannot re-enter itself
      however its actions cascade }
    W.Fired[I] := True;
    RunActions(W, W.Events[I], O);
  end;

  Dec(Depth);
end;

end.
