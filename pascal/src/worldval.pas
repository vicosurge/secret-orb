{ worldval.pas - World validation for the Secret Orb editors }
{
  Reports the mistakes an author can make that the engine cannot complain
  about at run time: an exit into nothing, a room nobody can reach, a
  paragraph number that names an empty slot.

  Editor-only by design. secretorb.pas must not use this unit - the game runs
  from a 720KB floppy and has no business carrying checks that belong to the
  authoring side. The rules here mirror validate() in web/editor.html so that
  all three editors agree about what is wrong with a world.
}
unit WorldVal;

{$MODE OBJFPC}

interface

uses
  SysUtils, GameData, DataFile;   { DataFile is where FindRoomByID lives }

const
  MAX_ISSUES = 128;

type
  TIssueLevel = (ilError, ilWarn);

  TIssue = record
    Level: TIssueLevel;
    Text: string[100];
    Where: string[20];
  end;

  TIssueList = array[1..MAX_ISSUES] of TIssue;

{ Fills List and returns how many issues were found. Stops at MAX_ISSUES; a
  world that bad has bigger problems than an exhaustive report. }
function ValidateWorld(var W: TGameWorld; var List: TIssueList): Integer;
function IssueLevelName(L: TIssueLevel): string;

{ Counts the exits of a room whose opposite side is free, and with Apply set,
  fills them in. Both editors call this so they offer identical behaviour, and
  calling it with Apply False is how they decide whether to prompt at all. }
function PairExits(var W: TGameWorld; RoomIdx: Integer;
                   Apply: Boolean): Integer;

{ Writes an author's cross-reference: which trigger fires each paragraph, which
  paragraphs nothing fires, and which triggers name an empty slot. Deliberately
  a separate file from the booklet - the booklet goes to the player, and a list
  of what fires when would give the game away. }
function WriteParaXRef(const FileName: string; var W: TGameWorld): Boolean;

implementation

type
  TValState = record
    List: ^TIssueList;
    Count: Integer;
  end;

var
  { Reachability scratch, indexed by room array position. Held here rather
    than on the stack for the same reason display.pas keeps its wrap buffer
    global: DOS stacks are small. }
  Seen: array[1..MAX_ROOMS] of Boolean;
  Queue: array[1..MAX_ROOMS] of Integer;

procedure Add(var S: TValState; L: TIssueLevel;
              const Text, Where: string);
begin
  if S.Count >= MAX_ISSUES then Exit;
  Inc(S.Count);
  S.List^[S.Count].Level := L;
  S.List^[S.Count].Text := Text;
  S.List^[S.Count].Where := Where;
end;

function RoomLabel(ID: Word): string;
begin
  Result := 'room ' + IntToStr(ID);
end;

{ A paragraph number that is set but names an empty slot fires nothing at all
  at run time - ShowParagraph exits silently on an empty body - so this is the
  one authoring mistake with no symptom whatsoever in the game. }
procedure CheckPara(var S: TValState; var W: TGameWorld;
                    Num: Word; const What, Where: string);
begin
  if Num = 0 then Exit;
  if Num > MAX_PARAGRAPHS then
    Add(S, ilError, What + ' is paragraph ' + IntToStr(Num) +
        ', above the limit of ' + IntToStr(MAX_PARAGRAPHS) + '.', Where)
  else if ParagraphText(W, Num) = '' then
    Add(S, ilError, What + ' names paragraph ' + IntToStr(Num) +
        ', which is empty.', Where);
end;

procedure CheckLength(var S: TValState; Len, Max: Integer;
                      const What, Where: string);
begin
  if Len > Max then
    Add(S, ilWarn, What + ' is ' + IntToStr(Len) + ' characters; it will be ' +
        'cut to ' + IntToStr(Max) + '.', Where);
end;

procedure CheckIDs(var S: TValState; var W: TGameWorld);
var
  I, J: Integer;
begin
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
    begin
      if W.Rooms[I].ID = 0 then
        Add(S, ilError, 'Room has ID 0, which the engine reads as "nowhere".',
            'room ' + IntToStr(I));
      for J := I + 1 to MAX_ROOMS do
        if W.Rooms[J].Active and (W.Rooms[J].ID = W.Rooms[I].ID) then
        begin
          Add(S, ilError, 'Room ID ' + IntToStr(W.Rooms[I].ID) +
              ' is used more than once.', RoomLabel(W.Rooms[I].ID));
          Break;
        end;
    end;

  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      if W.Objects[I].ID = 0 then
        Add(S, ilError, 'Object has ID 0, which the engine reads as "nowhere".',
            'object ' + IntToStr(I));
      for J := I + 1 to MAX_OBJECTS do
        if W.Objects[J].Active and (W.Objects[J].ID = W.Objects[I].ID) then
        begin
          Add(S, ilError, 'Object ID ' + IntToStr(W.Objects[I].ID) +
              ' is used more than once.',
              'object ' + IntToStr(W.Objects[I].ID));
          Break;
        end;
    end;

  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
    begin
      if W.Mobs[I].ID = 0 then
        Add(S, ilError, 'Mob has ID 0, which the engine reads as "nowhere".',
            'mob ' + IntToStr(I));
      for J := I + 1 to MAX_MOBS do
        if W.Mobs[J].Active and (W.Mobs[J].ID = W.Mobs[I].ID) then
        begin
          Add(S, ilError, 'Mob ID ' + IntToStr(W.Mobs[I].ID) +
              ' is used more than once.', 'mob ' + IntToStr(W.Mobs[I].ID));
          Break;
        end;
    end;
end;

procedure CheckContents(var S: TValState; var W: TGameWorld);
var
  I: Integer;
  Where: string;
begin
  CheckLength(S, Length(W.Title), MAX_NAME_LEN, 'World title', 'world');

  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
    begin
      Where := RoomLabel(W.Rooms[I].ID);
      if Trim(W.Rooms[I].Name) = '' then
        Add(S, ilWarn, 'Room has no name.', Where);
      CheckLength(S, Length(W.Rooms[I].Name), MAX_NAME_LEN, 'Room name', Where);
      CheckLength(S, Length(W.Rooms[I].Desc), MAX_DESC_LEN,
                  'Room description', Where);
      CheckPara(S, W, W.Rooms[I].FirstVisitPara, 'First visit', Where);
    end;

  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      Where := 'object ' + IntToStr(W.Objects[I].ID);
      if Trim(W.Objects[I].Name) = '' then
        Add(S, ilWarn, 'Object has no name.', Where);
      CheckLength(S, Length(W.Objects[I].Name), MAX_OBJ_NAME,
                  'Object name', Where);
      CheckLength(S, Length(W.Objects[I].Desc), MAX_OBJ_DESC,
                  'Object description', Where);
      CheckLength(S, Length(W.Objects[I].UseText), MAX_OBJ_DESC,
                  'Use text', Where);
      CheckPara(S, W, W.Objects[I].FirstTakePara, 'First take', Where);
    end;

  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
    begin
      Where := 'mob ' + IntToStr(W.Mobs[I].ID);
      if Trim(W.Mobs[I].Name) = '' then
        Add(S, ilWarn, 'Mob has no name.', Where);
      CheckLength(S, Length(W.Mobs[I].Name), MAX_OBJ_NAME, 'Mob name', Where);
      CheckLength(S, Length(W.Mobs[I].Desc), MAX_OBJ_DESC,
                  'Mob description', Where);
      CheckLength(S, Length(W.Mobs[I].Dialogue), MAX_DIALOGUE,
                  'Dialogue', Where);
      CheckPara(S, W, W.Mobs[I].FirstTalkPara, 'First talk', Where);
    end;
end;

procedure CheckReferences(var S: TValState; var W: TGameWorld);
var
  I: Integer;
  D: TDirection;
  Target: Word;
  Where: string;
begin
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
    begin
      Where := RoomLabel(W.Rooms[I].ID);
      for D := Low(TDirection) to High(TDirection) do
      begin
        Target := W.Rooms[I].Exits[D];
        if (Target <> DIR_NONE) and (FindRoomByID(W, Target) < 0) then
          Add(S, ilError, 'The ' + GetExitName(D) + ' exit points at room ' +
              IntToStr(Target) + ', which does not exist.', Where);
      end;
    end;

  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      Where := 'object ' + IntToStr(W.Objects[I].ID);
      if (W.Objects[I].RoomID <> 0) and
         (FindRoomByID(W, W.Objects[I].RoomID) < 0) then
        Add(S, ilError, 'Object sits in room ' +
            IntToStr(W.Objects[I].RoomID) + ', which does not exist.', Where);
      if (W.Objects[I].CarriedBy <> 0) and
         (FindMobByID(W, W.Objects[I].CarriedBy) < 0) then
        Add(S, ilError, 'Object is carried by mob ' +
            IntToStr(W.Objects[I].CarriedBy) + ', which does not exist.',
            Where);
      if (W.Objects[I].RoomID = 0) and (W.Objects[I].CarriedBy = 0) then
        Add(S, ilWarn, 'Object is in room 0 and carried by nobody, so the ' +
            'player can never find it.', Where);
    end;

  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
    begin
      Where := 'mob ' + IntToStr(W.Mobs[I].ID);
      if (W.Mobs[I].RoomID = 0) or (FindRoomByID(W, W.Mobs[I].RoomID) < 0) then
        Add(S, ilError, 'Mob is in room ' + IntToStr(W.Mobs[I].RoomID) +
            ', which does not exist.', Where);
    end;
end;

procedure CheckWinAndStart(var S: TValState; var W: TGameWorld);
var
  Idx: Integer;
begin
  if FindRoomByID(W, W.CurrentRoom) < 0 then
    Add(S, ilError, 'Start room ' + IntToStr(W.CurrentRoom) +
        ' does not exist.', 'world');

  if W.WinRoomID = 0 then
  begin
    if W.RoomCount > 0 then
      Add(S, ilWarn, 'No win room is set, so the adventure cannot be ' +
          'completed.', 'world');
  end
  else if FindRoomByID(W, W.WinRoomID) < 0 then
    Add(S, ilError, 'Win room ' + IntToStr(W.WinRoomID) + ' does not exist.',
        'world');

  if W.WinObjectID <> 0 then
  begin
    Idx := FindObjectByID(W, W.WinObjectID);
    if Idx < 0 then
      Add(S, ilError, 'Win object ' + IntToStr(W.WinObjectID) +
          ' does not exist.', 'world')
    else if not (ofPickup in W.Objects[Idx].Flags) then
      Add(S, ilError, 'Win object "' + W.Objects[Idx].Name + '" cannot be ' +
          'picked up, so the win condition is unreachable.',
          'object ' + IntToStr(W.WinObjectID));
  end;

  CheckPara(S, W, W.IntroPara, 'Intro', 'world');
  CheckPara(S, W, W.WinPara, 'Win text', 'world');
  CheckPara(S, W, W.LosePara, 'Lose text', 'world');
end;

{ Breadth-first walk of the exits from the start room. Anything not touched is
  content the player can never see. }
procedure CheckReachability(var S: TValState; var W: TGameWorld);
var
  I, Head, Tail, Idx, Target: Integer;
  D: TDirection;
begin
  Idx := FindRoomByID(W, W.CurrentRoom);
  if Idx < 0 then Exit;

  for I := 1 to MAX_ROOMS do
    Seen[I] := False;

  Seen[Idx] := True;
  Queue[1] := Idx;
  Head := 1;
  Tail := 1;

  while Head <= Tail do
  begin
    Idx := Queue[Head];
    Inc(Head);
    for D := Low(TDirection) to High(TDirection) do
      if W.Rooms[Idx].Exits[D] <> DIR_NONE then
      begin
        Target := FindRoomByID(W, W.Rooms[Idx].Exits[D]);
        if (Target > 0) and not Seen[Target] then
        begin
          Seen[Target] := True;
          Inc(Tail);
          Queue[Tail] := Target;
        end;
      end;
  end;

  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active and not Seen[I] then
      Add(S, ilError, 'Room "' + W.Rooms[I].Name + '" cannot be reached ' +
          'from the start room.', RoomLabel(W.Rooms[I].ID));

  if W.WinRoomID <> 0 then
  begin
    Idx := FindRoomByID(W, W.WinRoomID);
    if (Idx > 0) and not Seen[Idx] then
      Add(S, ilError, 'The win room cannot be reached from the start room.',
          'world');
  end;
end;

{ One-way exits are legal - a trapdoor needs one - but far more often they are
  an exit somebody forgot to pair up, so they are a warning, not an error. }
procedure CheckOneWayExits(var S: TValState; var W: TGameWorld);
var
  I, Other: Integer;
  D: TDirection;
begin
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
      for D := Low(TDirection) to High(TDirection) do
        if W.Rooms[I].Exits[D] <> DIR_NONE then
        begin
          Other := FindRoomByID(W, W.Rooms[I].Exits[D]);
          if (Other > 0) and
             (W.Rooms[Other].Exits[OppositeDir(D)] <> W.Rooms[I].ID) then
            Add(S, ilWarn, 'The ' + GetExitName(D) + ' exit to room ' +
                IntToStr(W.Rooms[I].Exits[D]) + ' has no matching way back.',
                RoomLabel(W.Rooms[I].ID));
        end;
end;

procedure CheckCapacity(var S: TValState; var W: TGameWorld);
var
  I, Takeable: Integer;
begin
  Takeable := 0;
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active and (ofPickup in W.Objects[I].Flags) then
      Inc(Takeable);

  if Takeable > MAX_INVENTORY then
    Add(S, ilWarn, 'There are ' + IntToStr(Takeable) + ' takeable objects ' +
        'but the player can only carry ' + IntToStr(MAX_INVENTORY) +
        ' at a time.', 'world');
end;

{ ---- Events -------------------------------------------------------------

  Everything an event names is a number, and a number that names nothing is
  invisible at run time: FireEvents skips a trigger that never matches and
  RunActions skips an action whose target is not there. Neither says a word.
  That makes events the largest source of the mistake this unit exists for.

  The rules follow the interpreter in events.pas exactly, so they have to be
  read together. Three of its conventions matter here:

    - a TriggerID of 0 means "any", so an author can write one event for
      every object. It is not a missing reference;
    - a TriggerID2 that is set on a trigger whose hook passes 0 can never
      match, so the event is dead - an error, not a warning;
    - FireEvents stops at W.EventCount, so an active event above it is dead
      too. }

function EventLabel(N: Integer): string;
begin
  Result := 'event ' + IntToStr(N);
end;

procedure NeedRoom(var S: TValState; var W: TGameWorld;
                   ID: Word; const What, Where: string);
begin
  if FindRoomByID(W, ID) < 0 then
    Add(S, ilError, What + ' names room ' + IntToStr(ID) +
        ', which does not exist.', Where);
end;

procedure NeedObject(var S: TValState; var W: TGameWorld;
                     ID: Word; const What, Where: string);
begin
  if FindObjectByID(W, ID) < 0 then
    Add(S, ilError, What + ' names object ' + IntToStr(ID) +
        ', which does not exist.', Where);
end;

procedure NeedMob(var S: TValState; var W: TGameWorld;
                  ID: Word; const What, Where: string);
begin
  if FindMobByID(W, ID) < 0 then
    Add(S, ilError, What + ' names mob ' + IntToStr(ID) +
        ', which does not exist.', Where);
end;

procedure NeedFlag(var S: TValState; ID: Word;
                   const What, Where: string);
begin
  if (ID < 1) or (ID > MAX_FLAGS) then
    Add(S, ilError, What + ' names flag ' + IntToStr(ID) + '; flags run 1 to ' +
        IntToStr(MAX_FLAGS) + '.', Where);
end;

procedure NeedCounter(var S: TValState; ID: Word;
                      const What, Where: string);
begin
  if (ID < 1) or (ID > MAX_COUNTERS) then
    Add(S, ilError, What + ' names counter ' + IntToStr(ID) +
        '; counters run 1 to ' + IntToStr(MAX_COUNTERS) + '.', Where);
end;

{ A trigger whose secondary ID the engine always passes as 0. Setting it
  makes TriggerMatches fail every time, so the event simply never runs. }
procedure NoSecondID(var S: TValState; const E: TWorldEvent;
                     const Where: string);
begin
  if E.TriggerID2 <> 0 then
    Add(S, ilError, 'Trigger ' + TriggerName(E.TriggerType) +
        ' has no second ID, so this event never fires.', Where);
end;

{ Does any action anywhere write this flag? Used to catch an etFlagSet
  trigger waiting on a flag nothing ever sets - the event equivalent of an
  unreachable room. }
function AnyActionWritesFlag(var W: TGameWorld; N: Word;
                             Want: Boolean): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  for I := 1 to MAX_EVENTS do
    if W.Events[I].Active then
      for J := 1 to W.Events[I].ActionCount do
        with W.Events[I].Actions[J] do
          if TargetID = N then
            case ActionType of
              atToggleFlag: Exit;
              atSetFlag:    if Want then Exit;
              atClearFlag:  if not Want then Exit;
            end;
  Result := False;
end;

function AnyActionEnablesEvent(var W: TGameWorld; N: Word): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  for I := 1 to MAX_EVENTS do
    if W.Events[I].Active then
      for J := 1 to W.Events[I].ActionCount do
        if (W.Events[I].Actions[J].ActionType = atEnableEvent) and
           (W.Events[I].Actions[J].TargetID = N) then Exit;
  Result := False;
end;

procedure CheckTrigger(var S: TValState; var W: TGameWorld;
                       const E: TWorldEvent; const Where: string);
begin
  case E.TriggerType of
    etEnterRoom, etFirstVisit:
      begin
        if E.TriggerID <> 0 then NeedRoom(S, W, E.TriggerID, 'Trigger', Where);
        NoSecondID(S, E, Where);
      end;
    etExitRoom:
      begin
        { The only trigger with two rooms: the one being left and the one
          being entered }
        if E.TriggerID <> 0 then NeedRoom(S, W, E.TriggerID, 'Trigger', Where);
        if E.TriggerID2 <> 0 then
          NeedRoom(S, W, E.TriggerID2, 'Trigger destination', Where);
      end;
    etTakeObject, etDropObject, etUseObject, etExamineObject:
      begin
        if E.TriggerID <> 0 then
          NeedObject(S, W, E.TriggerID, 'Trigger', Where);
        NoSecondID(S, E, Where);
      end;
    etUseObjectOn:
      begin
        { Both sides are objects. Handing something to a person is GIVE,
          which has its own trigger. }
        if E.TriggerID <> 0 then
          NeedObject(S, W, E.TriggerID, 'Trigger', Where);
        if E.TriggerID2 <> 0 then
          NeedObject(S, W, E.TriggerID2, 'Trigger target', Where);
      end;
    etTalkToMob:
      begin
        if E.TriggerID <> 0 then NeedMob(S, W, E.TriggerID, 'Trigger', Where);
        NoSecondID(S, E, Where);
      end;
    etGiveTo:
      begin
        if E.TriggerID <> 0 then
          NeedObject(S, W, E.TriggerID, 'Trigger', Where);
        if E.TriggerID2 <> 0 then
          NeedMob(S, W, E.TriggerID2, 'Trigger recipient', Where);
      end;
    etTimer:
      { TriggerID is the turn to fire at and TriggerID2 the repeat period.
        The hook runs after the turn counter is incremented, so turn 0 never
        arrives: with no period either, the event waits forever. }
      if (E.TriggerID = 0) and (E.TriggerID2 = 0) then
        Add(S, ilError, 'Timer fires at turn 0 and never repeats, so it ' +
            'never fires.', Where);
    etFlagSet, etFlagClear:
      begin
        { 0 means any flag, which is a legitimate catch-all }
        if E.TriggerID <> 0 then
        begin
          NeedFlag(S, E.TriggerID, 'Trigger', Where);
          if (E.TriggerID <= MAX_FLAGS) and
             not AnyActionWritesFlag(W, E.TriggerID,
                                     E.TriggerType = etFlagSet) then
            Add(S, ilWarn, 'Waits on flag ' + IntToStr(E.TriggerID) +
                ', which no action ever writes.', Where);
        end;
        NoSecondID(S, E, Where);
      end;
  end;
end;

procedure CheckConditions(var S: TValState; var W: TGameWorld;
                          const E: TWorldEvent; const Where: string);
var
  I: Integer;
  What: string;
begin
  for I := 1 to E.CondCount do
  begin
    What := 'Condition ' + IntToStr(I);
    case E.Conditions[I].CondType of
      ctNone:
        Add(S, ilWarn, What + ' is empty and always passes.', Where);
      ctHasObject:
        NeedObject(S, W, E.Conditions[I].TargetID, What, Where);
      ctObjectInRoom:
        begin
          NeedObject(S, W, E.Conditions[I].TargetID, What, Where);
          NeedRoom(S, W, Word(E.Conditions[I].Value), What + ' room', Where);
        end;
      ctMobInRoom:
        begin
          NeedMob(S, W, E.Conditions[I].TargetID, What, Where);
          NeedRoom(S, W, Word(E.Conditions[I].Value), What + ' room', Where);
        end;
      ctFlagIsSet, ctFlagIsClear:
        NeedFlag(S, E.Conditions[I].TargetID, What, Where);
      ctCounterEquals, ctCounterGreater, ctCounterLess:
        NeedCounter(S, E.Conditions[I].TargetID, What, Where);
      ctVisitedRoom, ctRoomIs:
        NeedRoom(S, W, E.Conditions[I].TargetID, What, Where);
    end;
  end;
end;

procedure CheckActions(var S: TValState; var W: TGameWorld;
                       const E: TWorldEvent; const Where: string);
var
  I: Integer;
  What: string;
  Dir: TDirection;
  Dest: Word;
begin
  if E.ActionCount = 0 then
    Add(S, ilWarn, 'Event has no actions, so firing it does nothing.', Where);

  for I := 1 to E.ActionCount do
  begin
    What := 'Action ' + IntToStr(I);
    with E.Actions[I] do
      case ActionType of
        atNone:
          Add(S, ilWarn, What + ' is empty and does nothing.', Where);
        atShowMessage:
          if Text = '' then
            Add(S, ilError, What + ' shows an empty message.', Where);
        atShowParagraph:
          if TargetID = 0 then
            Add(S, ilError, What + ' shows paragraph 0, which is no ' +
                'paragraph at all.', Where)
          else
            CheckPara(S, W, TargetID, What, Where);

        atSetFlag, atClearFlag, atToggleFlag:
          NeedFlag(S, TargetID, What, Where);
        atSetCounter, atAddCounter, atSubCounter:
          NeedCounter(S, TargetID, What, Where);

        atMoveObject, atSpawnObject:
          begin
            NeedObject(S, W, TargetID, What, Where);
            { Value is the destination room. Zero puts the object nowhere,
              which is what atRemoveObject is for and is more likely a
              forgotten field than a deliberate disappearance. }
            if Value = 0 then
              Add(S, ilWarn, What + ' moves the object to room 0, taking it ' +
                  'out of play.', Where)
            else
              NeedRoom(S, W, Word(Value), What + ' destination', Where);
          end;
        atRemoveObject:
          NeedObject(S, W, TargetID, What, Where);
        atMoveMob:
          begin
            NeedMob(S, W, TargetID, What, Where);
            if Value = 0 then
              Add(S, ilWarn, What + ' moves the mob to room 0, taking it ' +
                  'out of play.', Where)
            else
              NeedRoom(S, W, Word(Value), What + ' destination', Where);
          end;
        atRemoveMob:
          NeedMob(S, W, TargetID, What, Where);

        atLockExit, atUnlockExit:
          begin
            NeedRoom(S, W, TargetID, What, Where);
            { The direction is the low three bits of Value, so it can hold a
              number no direction answers to. DecodeExitValue clamps that to
              north rather than crashing - silently locking the wrong door. }
            if (Value and $07) > Ord(High(TDirection)) then
              Add(S, ilError, What + ' has direction ' +
                  IntToStr(Value and $07) + ', which is not a direction.',
                  Where)
            else
            begin
              DecodeExitValue(Value, Dir, Dest);
              if ActionType = atUnlockExit then
                if Dest = 0 then
                  Add(S, ilError, What + ' opens the ' + GetExitName(Dir) +
                      ' exit onto nothing.', Where)
                else
                  NeedRoom(S, W, Dest, What + ' destination', Where);
            end;
          end;

        atTeleportPlayer:
          NeedRoom(S, W, TargetID, What, Where);
        atAddScore:
          if TargetID = 0 then
            Add(S, ilWarn, What + ' awards 0 points.', Where);
        atEndGame: ;

        atEnableEvent, atDisableEvent:
          if (TargetID < 1) or (TargetID > MAX_EVENTS) then
            Add(S, ilError, What + ' names event ' + IntToStr(TargetID) +
                '; events run 1 to ' + IntToStr(MAX_EVENTS) + '.', Where)
          else if not W.Events[TargetID].Active then
            Add(S, ilError, What + ' names event ' + IntToStr(TargetID) +
                ', which is an empty slot.', Where);
      end;
  end;
end;

procedure CheckEvents(var S: TValState; var W: TGameWorld);
var
  I: Integer;
  Where: string;
begin
  for I := 1 to MAX_EVENTS do
  begin
    if not W.Events[I].Active then Continue;
    Where := EventLabel(I);

    { FireEvents walks slots 1..EventCount, so anything past it is dead
      whatever it says. The loader keeps EventCount at the highest used slot,
      so this catches an editor that forgot to raise it. }
    if I > W.EventCount then
      Add(S, ilError, 'Event is above the event count of ' +
          IntToStr(W.EventCount) + ', so it never fires.', Where);

    if W.Events[I].Name = '' then
      Add(S, ilWarn, 'Event has no name, which makes it hard to find.',
          Where);

    if not W.Events[I].Enabled then
      if not AnyActionEnablesEvent(W, I) then
        Add(S, ilWarn, 'Event starts disabled and nothing enables it.',
            Where);

    CheckTrigger(S, W, W.Events[I], Where);
    CheckConditions(S, W, W.Events[I], Where);
    CheckActions(S, W, W.Events[I], Where);
  end;
end;

function ValidateWorld(var W: TGameWorld; var List: TIssueList): Integer;
var
  S: TValState;
begin
  S.List := @List;
  S.Count := 0;

  CheckIDs(S, W);
  CheckContents(S, W);
  CheckReferences(S, W);
  CheckWinAndStart(S, W);
  CheckReachability(S, W);
  CheckOneWayExits(S, W);
  CheckCapacity(S, W);
  CheckEvents(S, W);

  Result := S.Count;
end;

function IssueLevelName(L: TIssueLevel): string;
begin
  if L = ilError then
    Result := 'ERROR'
  else
    Result := 'warn';
end;

{ Only a free opposite exit is filled. One that already leads somewhere else is
  left alone - that is either a deliberate one-way link or a mistake
  CheckOneWayExits will report, and neither should be overwritten silently. A
  room whose exit loops back to itself is skipped for the same reason. }
function PairExits(var W: TGameWorld; RoomIdx: Integer;
                   Apply: Boolean): Integer;
var
  D: TDirection;
  Other: Integer;
begin
  Result := 0;
  if (RoomIdx < 1) or (RoomIdx > MAX_ROOMS) then Exit;
  if not W.Rooms[RoomIdx].Active then Exit;

  for D := Low(TDirection) to High(TDirection) do
  begin
    if W.Rooms[RoomIdx].Exits[D] = DIR_NONE then Continue;
    Other := FindRoomByID(W, W.Rooms[RoomIdx].Exits[D]);
    if (Other <= 0) or (Other = RoomIdx) then Continue;
    if W.Rooms[Other].Exits[OppositeDir(D)] <> DIR_NONE then Continue;

    Inc(Result);
    if Apply then
      W.Rooms[Other].Exits[OppositeDir(D)] := W.Rooms[RoomIdx].ID;
  end;
end;

{ Names every trigger that fires paragraph Num, as one line each. Returns how
  many were found, so the caller can flag a paragraph nothing reaches. }
function DescribeTriggers(var F: Text; var W: TGameWorld; Num: Word): Integer;
var
  I, J: Integer;
begin
  Result := 0;

  if W.IntroPara = Num then
  begin
    WriteLn(F, '      fired by: the world intro');
    Inc(Result);
  end;
  if W.WinPara = Num then
  begin
    WriteLn(F, '      fired by: winning');
    Inc(Result);
  end;
  if W.LosePara = Num then
  begin
    WriteLn(F, '      fired by: ending without a win');
    Inc(Result);
  end;

  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active and (W.Rooms[I].FirstVisitPara = Num) then
    begin
      WriteLn(F, '      fired by: first visit to room ',
              W.Rooms[I].ID, ' "', W.Rooms[I].Name, '"');
      Inc(Result);
    end;

  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active and (W.Objects[I].FirstTakePara = Num) then
    begin
      WriteLn(F, '      fired by: first take of object ',
              W.Objects[I].ID, ' "', W.Objects[I].Name, '"');
      Inc(Result);
    end;

  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active and (W.Mobs[I].FirstTalkPara = Num) then
    begin
      WriteLn(F, '      fired by: first talk with mob ',
              W.Mobs[I].ID, ' "', W.Mobs[I].Name, '"');
      Inc(Result);
    end;

  { The seventh route to a paragraph, and the only one that is not a field on
    an entity: an event's atShowParagraph action. Without this an
    event-fired paragraph is reported as "fired by: NOTHING", which is the
    one line in this file an author is meant to act on. }
  for I := 1 to MAX_EVENTS do
    if W.Events[I].Active then
      for J := 1 to W.Events[I].ActionCount do
        if (W.Events[I].Actions[J].ActionType = atShowParagraph) and
           (W.Events[I].Actions[J].TargetID = Num) then
        begin
          WriteLn(F, '      fired by: event ', I, ' "', W.Events[I].Name,
                  '", action ', J, ' (', TriggerName(W.Events[I].TriggerType),
                  ')');
          Inc(Result);
        end;
end;

{ Reports a trigger pointing at a slot with no text. At run time this fires
  nothing and says nothing, so the cross-reference is where an author finds it. }
procedure ReportDangling(var F: Text; var W: TGameWorld; var Count: Integer;
                         Num: Word; const What: string);
begin
  if Num = 0 then Exit;
  if (Num <= MAX_PARAGRAPHS) and (ParagraphText(W, Num) <> '') then Exit;
  WriteLn(F, '  ', What, ' names paragraph ', Num, ', which is empty.');
  Inc(Count);
end;

function WriteParaXRef(const FileName: string; var W: TGameWorld): Boolean;
var
  F: Text;
  I, J, Used, Orphans, Dangling: Integer;
  Preview: TParaText;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F);
  {$I+}
  if IOResult <> 0 then Exit;

  WriteLn(F, W.Title, ' - paragraph cross-reference');
  WriteLn(F, StringOfChar('=', Length(W.Title) + 29));
  WriteLn(F);
  WriteLn(F, 'For the author, not the player: it says what fires each');
  WriteLn(F, 'paragraph. The numbers match the ones the game cites and the');
  WriteLn(F, 'booklet prints.');
  WriteLn(F);

  Used := 0;
  Orphans := 0;
  for I := 1 to W.ParaCount do
    if W.Paragraphs[I] <> '' then
    begin
      Inc(Used);
      Preview := Copy(W.Paragraphs[I], 1, 60);
      { A hard break inside the preview would split the line in two }
      if Pos(#13, Preview) > 0 then Preview := Copy(Preview, 1, Pos(#13, Preview) - 1);
      if Pos(#10, Preview) > 0 then Preview := Copy(Preview, 1, Pos(#10, Preview) - 1);
      WriteLn(F, '  ', I:4, '  ', Preview, '...');
      if DescribeTriggers(F, W, I) = 0 then
      begin
        WriteLn(F, '      fired by: NOTHING - no trigger names this paragraph');
        Inc(Orphans);
      end;
      WriteLn(F);
    end;

  if Used = 0 then
    WriteLn(F, '  (no paragraphs)');

  WriteLn(F);
  WriteLn(F, 'Triggers naming an empty paragraph');
  WriteLn(F, '----------------------------------');
  Dangling := 0;
  ReportDangling(F, W, Dangling, W.IntroPara, 'The world intro');
  ReportDangling(F, W, Dangling, W.WinPara, 'The win text');
  ReportDangling(F, W, Dangling, W.LosePara, 'The lose text');
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
      ReportDangling(F, W, Dangling, W.Rooms[I].FirstVisitPara,
                     'Room ' + IntToStr(W.Rooms[I].ID) + ' first visit');
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
      ReportDangling(F, W, Dangling, W.Objects[I].FirstTakePara,
                     'Object ' + IntToStr(W.Objects[I].ID) + ' first take');
  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
      ReportDangling(F, W, Dangling, W.Mobs[I].FirstTalkPara,
                     'Mob ' + IntToStr(W.Mobs[I].ID) + ' first talk');
  for I := 1 to MAX_EVENTS do
    if W.Events[I].Active then
      for J := 1 to W.Events[I].ActionCount do
        if W.Events[I].Actions[J].ActionType = atShowParagraph then
          ReportDangling(F, W, Dangling, W.Events[I].Actions[J].TargetID,
                         'Event ' + IntToStr(I) + ' action ' + IntToStr(J));
  if Dangling = 0 then
    WriteLn(F, '  (none)');

  WriteLn(F);
  WriteLn(F, Used, ' paragraph(s), ', Orphans, ' never fired, ',
          Dangling, ' trigger(s) naming an empty slot.');

  Close(F);
  Result := IOResult = 0;
end;

end.
