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

type
  TDirection = (dirNorth, dirSouth, dirEast, dirWest, dirUp, dirDown);

  TRoom = record
    ID: Word;
    Name: string[MAX_NAME_LEN];
    Desc: string[MAX_DESC_LEN];
    Exits: array[TDirection] of Word;
    Active: Boolean;
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
    Active: Boolean;
  end;

  TMob = record
    ID: Word;
    Name: string[MAX_OBJ_NAME];
    Desc: string[MAX_OBJ_DESC];
    RoomID: Word;
    Dialogue: string[MAX_DIALOGUE];
    Active: Boolean;
  end;

  TInventory = array[1..MAX_INVENTORY] of Word;

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
function FindObjectByName(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
function FindMobByName(var W: TGameWorld; RoomID: Word; const Name: string): Integer;

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
  R.Active := False;
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
  O.Active := False;
end;

procedure InitMob(var M: TMob);
begin
  M.ID := 0;
  M.Name := '';
  M.Desc := '';
  M.RoomID := 0;
  M.Dialogue := '';
  M.Active := False;
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
  if (U = 'U') or (U = 'UP') then
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

function FindObjectByName(var W: TGameWorld; RoomID: Word; const Name: string): Integer;
var
  I: Integer;
  SearchName, ObjName: string;
begin
  Result := -1;
  SearchName := StrUpper(Name);
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      ObjName := StrUpper(W.Objects[I].Name);
      { Match if object is in room or player inventory }
      if ((W.Objects[I].RoomID = RoomID) and (W.Objects[I].CarriedBy = 0)) or
         ((W.Objects[I].RoomID = 0) and (W.Objects[I].CarriedBy = 0)) then
        if Pos(SearchName, ObjName) > 0 then
        begin
          Result := I;
          Exit;
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
