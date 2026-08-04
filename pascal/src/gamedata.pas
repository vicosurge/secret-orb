{ gamedata.pas - Core data structures for Secret Orb }
unit GameData;

{$MODE OBJFPC}

interface

const
  MAX_ROOMS = 256;
  MAX_OBJECTS = 128;
  MAX_MOBS = 64;
  MAX_INVENTORY = 8;
  MAX_NAME_LEN = 40;
  MAX_DESC_LEN = 255;
  MAX_OBJ_NAME = 30;
  MAX_OBJ_DESC = 100;
  MAX_DIALOGUE = 200;
  DIR_NONE = 0;
  { Story paragraphs. Numbers are printed in the booklet, so a paragraph's
    number is its array index and never shifts when another is deleted. }
  MAX_PARAGRAPHS = 128;
  MAX_PARA_LINES = 20;      { Editing grid height in the TUI editors }
  MAX_PARA_LEN = 1600;      { About MAX_PARA_LINES x 76 columns }
  { TGameWorld.WorldFlags bits }
  WF_BOOKLET = $01;         { Cite paragraph numbers instead of printing them }
  { Events. MAX_EVENTS is 128 rather than the 256 the design document names,
    and an action's inline text is a short string rather than a plain "string":
    these units compile with short strings on, so a bare "string" field is a
    256-byte ShortString, and 256 events x 8 actions of those would be half a
    megabyte of static data. Keeping TAction a fixed record - no AnsiString -
    is also what lets an event be FillChar-zeroed and BlockWrite-n like every
    other record here, which is what keeps world files reproducible. Prose too
    long for MAX_EVENT_TEXT belongs in a paragraph; atShowParagraph cites one. }
  MAX_EVENTS = 128;
  MAX_FLAGS = 64;
  MAX_COUNTERS = 32;
  MAX_CONDITIONS = 4;       { ANDed together }
  MAX_ACTIONS = 8;
  MAX_EVENT_TEXT = 80;      { One line of inline message }
  MAX_EVENT_NAME = 40;      { Editor display only, never shown to the player }
  MAX_VAR_NAME = 30;        { Flag and counter names, also editor-only }

type
  TDirection = (dirNorth, dirSouth, dirEast, dirWest, dirUp, dirDown);

  TRoom = record
    ID: Word;
    Name: string[MAX_NAME_LEN];
    Desc: string[MAX_DESC_LEN];
    Exits: array[TDirection] of Word;
    Points: Word;           { Awarded on first visit }
    Active: Boolean;
    FirstVisitPara: Word;   { Paragraph shown on first visit, 0 = none }
  end;

  TRoomArray = array[1..MAX_ROOMS] of TRoom;

  TObjectFlag = (ofPickup, ofUse, ofOpen, ofRead);
  TObjectFlags = set of TObjectFlag;

  TGameObject = record
    ID: Word;
    Name: string[MAX_OBJ_NAME];
    Desc: string[MAX_OBJ_DESC];
    RoomID: Word;           { 0 = in inventory }
    CarriedBy: Word;        { 0 = room/player, else mob ID }
    Flags: TObjectFlags;
    UseText: string[MAX_OBJ_DESC];
    Points: Word;           { Awarded on first take }
    Active: Boolean;
    FirstTakePara: Word;    { Paragraph shown on first take, 0 = none }
  end;

  TMob = record
    ID: Word;
    Name: string[MAX_OBJ_NAME];
    Desc: string[MAX_OBJ_DESC];
    RoomID: Word;
    Dialogue: string[MAX_DIALOGUE];
    Active: Boolean;
    FirstTalkPara: Word;    { Paragraph shown on first talk, 0 = none }
  end;

  TInventory = array[1..MAX_INVENTORY] of Word;
  TVisitedArray = array[1..MAX_ROOMS] of Boolean;
  TTakenArray = array[1..MAX_OBJECTS] of Boolean;
  TTalkedArray = array[1..MAX_MOBS] of Boolean;
  { Explicitly AnsiString: this unit compiles with short strings on, where a
    plain "string" is a 256-byte ShortString. A paragraph is longer than that,
    and an array of them would be static bulk, not a table of pointers. }
  TParaText = AnsiString;
  TParagraphArray = array[1..MAX_PARAGRAPHS] of TParaText;

  { --- Events --------------------------------------------------------------
    An event is a trigger, up to MAX_CONDITIONS conditions that all have to
    hold, and up to MAX_ACTIONS actions run in order. The record is named
    TWorldEvent, not TEvent: editor-tv.pas uses both this unit and Turbo
    Vision, whose own TEvent would collide. }
  TEventTrigger = (etEnterRoom, etExitRoom, etFirstVisit,
                   etTakeObject, etDropObject,
                   etUseObject, etUseObjectOn, etExamineObject,
                   etTalkToMob, etGiveTo,
                   etTimer, etFlagSet, etFlagClear);

  TConditionType = (ctNone, ctHasObject, ctObjectInRoom, ctMobInRoom,
                    ctFlagIsSet, ctFlagIsClear,
                    ctCounterEquals, ctCounterGreater, ctCounterLess,
                    ctVisitedRoom, ctRoomIs);

  TCondition = packed record
    CondType: TConditionType;  { ctNone = unused slot }
    TargetID: Word;            { Object / mob / room / flag / counter }
    Value: SmallInt;           { Comparison value for the counter tests }
    Negate: Boolean;           { NOT this condition }
  end;

  TActionType = (atNone, atShowMessage, atShowParagraph,
                 atSetFlag, atClearFlag, atToggleFlag,
                 atSetCounter, atAddCounter, atSubCounter,
                 atMoveObject, atRemoveObject, atSpawnObject,
                 atMoveMob, atRemoveMob,
                 atUnlockExit, atLockExit,
                 atTeleportPlayer, atAddScore, atEndGame,
                 atEnableEvent, atDisableEvent);

  TAction = packed record
    ActionType: TActionType;
    TargetID: Word;                { Object / mob / room / flag / paragraph }
    Value: SmallInt;               { Amount, secondary ID, or direction }
    Text: string[MAX_EVENT_TEXT];  { atShowMessage only; '' otherwise }
  end;

  { An event has no ID field: its slot number IS its identity, and slots are
    never shifted or reused, exactly as paragraph numbers are not. Paragraphs
    need that because the numbers are printed in a booklet; events need it for
    a harder reason - the Fired and EvEnabled bitmaps in a save game are
    indexed by slot, and atEnableEvent/atDisableEvent name a slot. Compacting
    on save would silently repoint every existing save at the wrong events. }
  TWorldEvent = record
    Name: string[MAX_EVENT_NAME];
    TriggerType: TEventTrigger;
    TriggerID: Word;               { Entity that fires it, or the turn number }
    TriggerID2: Word;              { Secondary entity, or the timer period }
    Conditions: array[1..MAX_CONDITIONS] of TCondition;
    CondCount: Byte;
    Actions: array[1..MAX_ACTIONS] of TAction;
    ActionCount: Byte;
    OneShot: Boolean;              { Fire at most once per game }
    Enabled: Boolean;              { INITIAL state; W.EvEnabled is the live one }
    Active: Boolean;               { Slot in use }
  end;

  TEventArray = array[1..MAX_EVENTS] of TWorldEvent;
  TFlagArray = array[1..MAX_FLAGS] of Boolean;
  TCounterArray = array[1..MAX_COUNTERS] of SmallInt;
  TEventBoolArray = array[1..MAX_EVENTS] of Boolean;
  TFlagNameArray = array[1..MAX_FLAGS] of string[MAX_VAR_NAME];
  TCounterNameArray = array[1..MAX_COUNTERS] of string[MAX_VAR_NAME];

  TGameWorld = record
    Rooms: TRoomArray;
    RoomCount: Word;
    CurrentRoom: Word;
    Title: string[MAX_NAME_LEN];
    Objects: array[1..MAX_OBJECTS] of TGameObject;
    ObjectCount: Word;
    Mobs: array[1..MAX_MOBS] of TMob;
    MobCount: Word;
    PlayerInventory: TInventory;
    PlayerInvCount: Byte;
    { World definition: how the adventure is won }
    WinRoomID: Word;        { 0 = world has no ending }
    WinObjectID: Word;      { 0 = reaching WinRoomID is enough }
    MaxScore: Word;         { Sum of all room and object points }
    { Story paragraphs. Indexed by paragraph number; '' = unused slot }
    Paragraphs: TParagraphArray;
    ParaCount: Word;        { Highest used slot, not a count of non-empty ones }
    IntroPara: Word;        { Shown before the first room, 0 = none }
    WinPara: Word;          { Shown on winning }
    LosePara: Word;         { Shown when the game ends without a win }
    WorldFlags: Byte;       { WF_* bits }
    { Events, and the names the author gave the flags and counters they use.
      The names are an authoring aid - the engine only ever indexes by number }
    Events: TEventArray;
    EventCount: Word;         { Highest used slot, not a count of active ones }
    FlagNames: TFlagNameArray;
    CounterNames: TCounterNameArray;
    { Player progress }
    Score: Word;
    Turns: Word;
    Visited: TVisitedArray;   { By room array index, not room ID }
    Taken: TTakenArray;       { By object array index - points award once only }
    Talked: TTalkedArray;     { By mob array index - first-talk scene fires once }
    { Event runtime state. Saved with the rest of the progress, so a restored
      game neither replays a one-shot scene nor forgets a flag it had set }
    Flags: TFlagArray;
    Counters: TCounterArray;
    Fired: TEventBoolArray;     { By event array index - gates OneShot }
    EvEnabled: TEventBoolArray; { Events can switch other events off }
  end;

procedure InitRoom(var R: TRoom);
procedure InitObject(var O: TGameObject);
procedure InitMob(var M: TMob);
procedure InitEvent(var E: TWorldEvent);
procedure InitWorld(var W: TGameWorld);
function GetExitName(Dir: TDirection): string;
function OppositeDir(Dir: TDirection): TDirection;
function ParseDirection(const S: string): TDirection;
function DirectionValid(Dir: TDirection): Boolean;
function FindObjectByID(var W: TGameWorld; ID: Word): Integer;
function FindMobByID(var W: TGameWorld; ID: Word): Integer;
function FindObjectInRoom(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
function FindObjectInInventory(var W: TGameWorld; const Name: string): Integer;
function FindObjectVisible(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
function FindMobByName(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
function PlayerHasObject(var W: TGameWorld; ID: Word): Boolean;
function ComputeMaxScore(var W: TGameWorld): Word;
function ParagraphText(var W: TGameWorld; Num: Word): TParaText;
procedure SetParagraph(var W: TGameWorld; Num: Word; const S: TParaText);
{ atLockExit and atUnlockExit need three numbers - room, direction and (for
  unlock) where the exit leads - and a TAction carries only TargetID and
  Value. The room goes in TargetID; the direction and destination share Value,
  the direction in the low three bits. Unlock has to name the destination
  because locking discards it. }
function EncodeExitValue(Dir: TDirection; Dest: Word): SmallInt;
procedure DecodeExitValue(V: SmallInt; var Dir: TDirection; var Dest: Word);

{ Copies the authored Enabled flags into the live EvEnabled bitmap. Called
  after loading a world, and again by a save-game restore only when the save
  predates the event state - otherwise the save's own bitmap wins. }
procedure SeedEventState(var W: TGameWorld);

{ Stable spellings for the event enums. These are what the text and BPL
  formats write and what the editors show, so they are part of the file
  format: renaming one silently changes the meaning of every world that used
  it. The From* functions return the inert member for anything unrecognised,
  which is how a file written by a newer build degrades rather than breaks. }
function TriggerName(T: TEventTrigger): string;
function TriggerFromName(const S: string): TEventTrigger;
function ConditionName(C: TConditionType): string;
function ConditionFromName(const S: string): TConditionType;
function ActionName(A: TActionType): string;
function ActionFromName(const S: string): TActionType;

implementation

procedure InitRoom(var R: TRoom);
var
  D: TDirection;
begin
  R.ID := 0;
  R.Name := '';
  R.Desc := '';
  for D := Low(TDirection) to High(TDirection) do
    R.Exits[D] := DIR_NONE;
  R.Points := 0;
  R.Active := False;
  R.FirstVisitPara := 0;
end;

procedure InitObject(var O: TGameObject);
begin
  O.ID := 0;
  O.Name := '';
  O.Desc := '';
  O.RoomID := 0;
  O.CarriedBy := 0;
  O.Flags := [];
  O.UseText := '';
  O.Points := 0;
  O.Active := False;
  O.FirstTakePara := 0;
end;

procedure InitMob(var M: TMob);
begin
  M.ID := 0;
  M.Name := '';
  M.Desc := '';
  M.RoomID := 0;
  M.Dialogue := '';
  M.Active := False;
  M.FirstTalkPara := 0;
end;

procedure InitEvent(var E: TWorldEvent);
var
  I: Integer;
begin
  E.Name := '';
  E.TriggerType := etEnterRoom;
  E.TriggerID := 0;
  E.TriggerID2 := 0;
  for I := 1 to MAX_CONDITIONS do
  begin
    E.Conditions[I].CondType := ctNone;
    E.Conditions[I].TargetID := 0;
    E.Conditions[I].Value := 0;
    E.Conditions[I].Negate := False;
  end;
  E.CondCount := 0;
  for I := 1 to MAX_ACTIONS do
  begin
    E.Actions[I].ActionType := atNone;
    E.Actions[I].TargetID := 0;
    E.Actions[I].Value := 0;
    E.Actions[I].Text := '';
  end;
  E.ActionCount := 0;
  E.OneShot := True;
  E.Enabled := True;
  E.Active := False;
end;

procedure InitWorld(var W: TGameWorld);
var
  I: Integer;
begin
  W.RoomCount := 0;
  W.CurrentRoom := 1;
  W.Title := 'Untitled';
  for I := 1 to MAX_ROOMS do
    InitRoom(W.Rooms[I]);
  W.ObjectCount := 0;
  for I := 1 to MAX_OBJECTS do
    InitObject(W.Objects[I]);
  W.MobCount := 0;
  for I := 1 to MAX_MOBS do
    InitMob(W.Mobs[I]);
  W.PlayerInvCount := 0;
  for I := 1 to MAX_INVENTORY do
    W.PlayerInventory[I] := 0;
  W.WinRoomID := 0;
  W.WinObjectID := 0;
  W.MaxScore := 0;
  for I := 1 to MAX_PARAGRAPHS do
    W.Paragraphs[I] := '';
  W.ParaCount := 0;
  W.IntroPara := 0;
  W.WinPara := 0;
  W.LosePara := 0;
  W.WorldFlags := 0;
  W.EventCount := 0;
  for I := 1 to MAX_EVENTS do
    InitEvent(W.Events[I]);
  for I := 1 to MAX_FLAGS do
    W.FlagNames[I] := '';
  for I := 1 to MAX_COUNTERS do
    W.CounterNames[I] := '';
  W.Score := 0;
  W.Turns := 0;
  for I := 1 to MAX_ROOMS do
    W.Visited[I] := False;
  for I := 1 to MAX_OBJECTS do
    W.Taken[I] := False;
  for I := 1 to MAX_MOBS do
    W.Talked[I] := False;
  for I := 1 to MAX_FLAGS do
    W.Flags[I] := False;
  for I := 1 to MAX_COUNTERS do
    W.Counters[I] := 0;
  for I := 1 to MAX_EVENTS do
  begin
    W.Fired[I] := False;
    { Enabled by default - atDisableEvent is what switches one off }
    W.EvEnabled[I] := True;
  end;
end;

function GetExitName(Dir: TDirection): string;
begin
  case Dir of
    dirNorth: Result := 'North';
    dirSouth: Result := 'South';
    dirEast:  Result := 'East';
    dirWest:  Result := 'West';
    dirUp:    Result := 'Up';
    dirDown:  Result := 'Down';
  else
    Result := '?';
  end;
end;

{ The way back. Used by the editors to offer a matching return exit and by the
  validator to spot exits that have none. }
function OppositeDir(Dir: TDirection): TDirection;
begin
  case Dir of
    dirNorth: Result := dirSouth;
    dirSouth: Result := dirNorth;
    dirEast:  Result := dirWest;
    dirWest:  Result := dirEast;
    dirUp:    Result := dirDown;
    dirDown:  Result := dirUp;
  else
    Result := Dir;
  end;
end;

function ParseDirection(const S: string): TDirection;
var
  U: string;
begin
  U := UpCase(S);
  if U = '' then
    Result := dirNorth
  else if (U = 'U') or (U = 'UP') then
    Result := dirUp
  else if (U = 'D') or (U = 'DOWN') then
    Result := dirDown
  else
    case U[1] of
      'N': Result := dirNorth;
      'S': Result := dirSouth;
      'E': Result := dirEast;
      'W': Result := dirWest;
    else
      Result := dirNorth;
    end;
end;

function DirectionValid(Dir: TDirection): Boolean;
begin
  Result := (Dir >= Low(TDirection)) and (Dir <= High(TDirection));
end;

function FindObjectByID(var W: TGameWorld; ID: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active and (W.Objects[I].ID = ID) then
    begin
      Result := I;
      Exit;
    end;
end;

function FindMobByID(var W: TGameWorld; ID: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active and (W.Mobs[I].ID = ID) then
    begin
      Result := I;
      Exit;
    end;
end;

function StrUpper(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if (Result[I] >= 'a') and (Result[I] <= 'z') then
      Result[I] := Chr(Ord(Result[I]) - 32);
end;

{ Objects lying loose in a specific room - not carried by anyone }
function FindObjectInRoom(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
var
  I: Integer;
  SearchName: string;
begin
  Result := -1;
  SearchName := StrUpper(Name);
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active and
       (W.Objects[I].RoomID = RoomID) and (W.Objects[I].CarriedBy = 0) then
      if Pos(SearchName, StrUpper(W.Objects[I].Name)) > 0 then
      begin
        Result := I;
        Exit;
      end;
end;

{ Walks the actual inventory slots, so objects merely stranded at RoomID 0
  are not reachable }
function FindObjectInInventory(var W: TGameWorld; const Name: string): Integer;
var
  I, Idx: Integer;
  SearchName: string;
begin
  Result := -1;
  SearchName := StrUpper(Name);
  for I := 1 to W.PlayerInvCount do
  begin
    Idx := FindObjectByID(W, W.PlayerInventory[I]);
    if Idx > 0 then
      if Pos(SearchName, StrUpper(W.Objects[Idx].Name)) > 0 then
      begin
        Result := Idx;
        Exit;
      end;
  end;
end;

{ Anything the player can refer to: the room first, then what they carry }
function FindObjectVisible(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
begin
  Result := FindObjectInRoom(W, RoomID, Name);
  if Result < 0 then
    Result := FindObjectInInventory(W, Name);
end;

function PlayerHasObject(var W: TGameWorld; ID: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to W.PlayerInvCount do
    if W.PlayerInventory[I] = ID then
    begin
      Result := True;
      Exit;
    end;
end;

function ComputeMaxScore(var W: TGameWorld): Word;
var
  I: Integer;
  Total: LongInt;
begin
  Total := 0;
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
      Total := Total + W.Rooms[I].Points;
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
      Total := Total + W.Objects[I].Points;
  if Total > High(Word) then
    Total := High(Word);
  Result := Word(Total);
end;

{ Out-of-range numbers read as empty so callers never have to range-check }
function ParagraphText(var W: TGameWorld; Num: Word): TParaText;
begin
  if (Num >= 1) and (Num <= MAX_PARAGRAPHS) then
    Result := W.Paragraphs[Num]
  else
    Result := '';
end;

{ Slots are never compacted: clearing one leaves a hole so that the numbers
  already printed in a booklet keep pointing at the same text. }
procedure SetParagraph(var W: TGameWorld; Num: Word; const S: TParaText);
var
  I: Integer;
begin
  if (Num < 1) or (Num > MAX_PARAGRAPHS) then Exit;
  if Length(S) > MAX_PARA_LEN then
    W.Paragraphs[Num] := Copy(S, 1, MAX_PARA_LEN)
  else
    W.Paragraphs[Num] := S;
  if (S <> '') and (Num > W.ParaCount) then
    W.ParaCount := Num
  else if (S = '') and (Num = W.ParaCount) then
  begin
    W.ParaCount := 0;
    for I := MAX_PARAGRAPHS downto 1 do
      if W.Paragraphs[I] <> '' then
      begin
        W.ParaCount := I;
        Break;
      end;
  end;
end;

function FindMobByName(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
var
  I: Integer;
  SearchName, MobName: string;
begin
  Result := -1;
  SearchName := StrUpper(Name);
  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active and (W.Mobs[I].RoomID = RoomID) then
    begin
      MobName := StrUpper(W.Mobs[I].Name);
      if Pos(SearchName, MobName) > 0 then
      begin
        Result := I;
        Exit;
      end;
    end;
end;

function TriggerName(T: TEventTrigger): string;
begin
  case T of
    etEnterRoom:     Result := 'ENTERROOM';
    etExitRoom:      Result := 'EXITROOM';
    etFirstVisit:    Result := 'FIRSTVISIT';
    etTakeObject:    Result := 'TAKEOBJECT';
    etDropObject:    Result := 'DROPOBJECT';
    etUseObject:     Result := 'USEOBJECT';
    etUseObjectOn:   Result := 'USEOBJECTON';
    etExamineObject: Result := 'EXAMINEOBJECT';
    etTalkToMob:     Result := 'TALKTOMOB';
    etGiveTo:        Result := 'GIVETO';
    etTimer:         Result := 'TIMER';
    etFlagSet:       Result := 'FLAGSET';
    etFlagClear:     Result := 'FLAGCLEAR';
  else
    Result := 'ENTERROOM';
  end;
end;

function TriggerFromName(const S: string): TEventTrigger;
var
  T: TEventTrigger;
  U: string;
begin
  U := StrUpper(S);
  for T := Low(TEventTrigger) to High(TEventTrigger) do
    if TriggerName(T) = U then
    begin
      Result := T;
      Exit;
    end;
  Result := etEnterRoom;
end;

function ConditionName(C: TConditionType): string;
begin
  case C of
    ctNone:           Result := 'NONE';
    ctHasObject:      Result := 'HASOBJECT';
    ctObjectInRoom:   Result := 'OBJECTINROOM';
    ctMobInRoom:      Result := 'MOBINROOM';
    ctFlagIsSet:      Result := 'FLAGISSET';
    ctFlagIsClear:    Result := 'FLAGISCLEAR';
    ctCounterEquals:  Result := 'COUNTEREQUALS';
    ctCounterGreater: Result := 'COUNTERGREATER';
    ctCounterLess:    Result := 'COUNTERLESS';
    ctVisitedRoom:    Result := 'VISITEDROOM';
    ctRoomIs:         Result := 'ROOMIS';
  else
    Result := 'NONE';
  end;
end;

function ConditionFromName(const S: string): TConditionType;
var
  C: TConditionType;
  U: string;
begin
  U := StrUpper(S);
  for C := Low(TConditionType) to High(TConditionType) do
    if ConditionName(C) = U then
    begin
      Result := C;
      Exit;
    end;
  Result := ctNone;
end;

function ActionName(A: TActionType): string;
begin
  case A of
    atNone:           Result := 'NONE';
    atShowMessage:    Result := 'SHOWMESSAGE';
    atShowParagraph:  Result := 'SHOWPARAGRAPH';
    atSetFlag:        Result := 'SETFLAG';
    atClearFlag:      Result := 'CLEARFLAG';
    atToggleFlag:     Result := 'TOGGLEFLAG';
    atSetCounter:     Result := 'SETCOUNTER';
    atAddCounter:     Result := 'ADDCOUNTER';
    atSubCounter:     Result := 'SUBCOUNTER';
    atMoveObject:     Result := 'MOVEOBJECT';
    atRemoveObject:   Result := 'REMOVEOBJECT';
    atSpawnObject:    Result := 'SPAWNOBJECT';
    atMoveMob:        Result := 'MOVEMOB';
    atRemoveMob:      Result := 'REMOVEMOB';
    atUnlockExit:     Result := 'UNLOCKEXIT';
    atLockExit:       Result := 'LOCKEXIT';
    atTeleportPlayer: Result := 'TELEPORTPLAYER';
    atAddScore:       Result := 'ADDSCORE';
    atEndGame:        Result := 'ENDGAME';
    atEnableEvent:    Result := 'ENABLEEVENT';
    atDisableEvent:   Result := 'DISABLEEVENT';
  else
    Result := 'NONE';
  end;
end;

function ActionFromName(const S: string): TActionType;
var
  A: TActionType;
  U: string;
begin
  U := StrUpper(S);
  for A := Low(TActionType) to High(TActionType) do
    if ActionName(A) = U then
    begin
      Result := A;
      Exit;
    end;
  Result := atNone;
end;

function EncodeExitValue(Dir: TDirection; Dest: Word): SmallInt;
begin
  Result := SmallInt(Ord(Dir) or (Dest shl 3));
end;

procedure DecodeExitValue(V: SmallInt; var Dir: TDirection; var Dest: Word);
var
  D: Integer;
begin
  D := V and $07;
  if D > Ord(High(TDirection)) then D := 0;
  Dir := TDirection(D);
  Dest := (V shr 3) and $1FFF;
end;

procedure SeedEventState(var W: TGameWorld);
var
  I: Integer;
begin
  for I := 1 to MAX_EVENTS do
  begin
    W.Fired[I] := False;
    W.EvEnabled[I] := W.Events[I].Enabled;
  end;
end;

end.
