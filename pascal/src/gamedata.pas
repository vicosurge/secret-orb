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
    { Player progress }
    Score: Word;
    Turns: Word;
    Visited: TVisitedArray;   { By room array index, not room ID }
    Taken: TTakenArray;       { By object array index - points award once only }
    Talked: TTalkedArray;     { By mob array index - first-talk scene fires once }
  end;

procedure InitRoom(var R: TRoom);
procedure InitObject(var O: TGameObject);
procedure InitMob(var M: TMob);
procedure InitWorld(var W: TGameWorld);
function GetExitName(Dir: TDirection): string;
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
  W.Score := 0;
  W.Turns := 0;
  for I := 1 to MAX_ROOMS do
    W.Visited[I] := False;
  for I := 1 to MAX_OBJECTS do
    W.Taken[I] := False;
  for I := 1 to MAX_MOBS do
    W.Talked[I] := False;
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

end.
