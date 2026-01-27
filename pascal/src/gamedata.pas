{ gamedata.pas - Core data structures for Secret Orb }
unit GameData;

{$MODE OBJFPC}

interface

const
  MAX_ROOMS = 256;
  MAX_NAME_LEN = 40;
  MAX_DESC_LEN = 255;
  DIR_NONE = 0;

type
  TDirection = (dirNorth, dirSouth, dirEast, dirWest);

  TRoom = record
    ID: Word;
    Name: string[MAX_NAME_LEN];
    Desc: string[MAX_DESC_LEN];
    Exits: array[TDirection] of Word;
    Active: Boolean;
  end;

  TRoomArray = array[1..MAX_ROOMS] of TRoom;

  TGameWorld = record
    Rooms: TRoomArray;
    RoomCount: Word;
    CurrentRoom: Word;
    Title: string[MAX_NAME_LEN];
  end;

procedure InitRoom(var R: TRoom);
procedure InitWorld(var W: TGameWorld);
function GetExitName(Dir: TDirection): string;
function ParseDirection(const S: string): TDirection;
function DirectionValid(Dir: TDirection): Boolean;

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

procedure InitWorld(var W: TGameWorld);
var
  I: Integer;
begin
  W.RoomCount := 0;
  W.CurrentRoom := 1;
  W.Title := 'Untitled';
  for I := 1 to MAX_ROOMS do
    InitRoom(W.Rooms[I]);
end;

function GetExitName(Dir: TDirection): string;
begin
  case Dir of
    dirNorth: Result := 'North';
    dirSouth: Result := 'South';
    dirEast:  Result := 'East';
    dirWest:  Result := 'West';
  else
    Result := '?';
  end;
end;

function ParseDirection(const S: string): TDirection;
var
  U: string;
begin
  U := UpCase(S[1]);
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

end.
