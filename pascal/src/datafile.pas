{ datafile.pas - Data file I/O for Secret Orb }
{ Supports three formats: Binary (SORB), Text (INI-style), and BPL }
unit DataFile;

{$MODE OBJFPC}

interface

uses
  SysUtils, GameData;

type
  TSaveFormat = (sfBinary, sfText, sfBPL);

function LoadWorld(const FileName: string; var W: TGameWorld): Boolean;
function SaveWorld(const FileName: string; var W: TGameWorld): Boolean;
function SaveWorldAs(const FileName: string; var W: TGameWorld; Format: TSaveFormat): Boolean;
function FindRoomByID(var W: TGameWorld; ID: Word): Integer;

{ Save games are separate from world files - see the SORS format below }
function SaveGameState(const FileName: string; var W: TGameWorld): Boolean;
function LoadGameState(const FileName: string; var W: TGameWorld): Boolean;

implementation

uses
  BPLPars;

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

function ParseKeyValue(const Line: string; var Key, Value: string): Boolean;
var
  P: Integer;
begin
  P := Pos('=', Line);
  if P > 0 then
  begin
    Key := Trim(Copy(Line, 1, P - 1));
    Value := Trim(Copy(Line, P + 1, Length(Line) - P));
    Result := True;
  end
  else
    Result := False;
end;

function FindRoomByID(var W: TGameWorld; ID: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active and (W.Rooms[I].ID = ID) then
    begin
      Result := I;
      Exit;
    end;
end;

function ParseObjectFlags(const S: string): TObjectFlags;
var
  Upper: string;
begin
  Result := [];
  Upper := UpperCase(S);
  if Pos('PICKUP', Upper) > 0 then Include(Result, ofPickup);
  if Pos('USE', Upper) > 0 then Include(Result, ofUse);
  if Pos('OPEN', Upper) > 0 then Include(Result, ofOpen);
  if Pos('READ', Upper) > 0 then Include(Result, ofRead);
end;

function FlagsToString(Flags: TObjectFlags): string;
var
  First: Boolean;
begin
  Result := '';
  First := True;
  if ofPickup in Flags then
  begin
    Result := 'pickup';
    First := False;
  end;
  if ofUse in Flags then
  begin
    if not First then Result := Result + ',';
    Result := Result + 'use';
    First := False;
  end;
  if ofOpen in Flags then
  begin
    if not First then Result := Result + ',';
    Result := Result + 'open';
    First := False;
  end;
  if ofRead in Flags then
  begin
    if not First then Result := Result + ',';
    Result := Result + 'read';
  end;
end;

type
  TSectionType = (secNone, secWorld, secRoom, secObject, secMob);
  TFileFormat = (ffText, ffBinary, ffBPL);

const
  SORB_MAGIC = 'SORB';
  SORS_MAGIC = 'SORS';      { Save games }
  FILE_VERSION = 2;         { Version 1 files still load }
  SAVE_VERSION = 1;

type
  { The magic + version prefix is identical in every version, so it can be read
    on its own before committing to a version-specific header layout }
  TVersionPrefix = packed record
    Magic: array[0..3] of Char;
    Version: Word;
  end;

  { --- Version 1 (legacy, read-only) --- }

  TGameHeader = packed record
    Magic: array[0..3] of Char;
    Version: Word;
    RoomCount: Word;
    ObjectCount: Word;
    MobCount: Word;
    StartRoom: Word;
    Reserved: array[0..5] of Byte;
  end;

  TRoomBin = packed record
    ID: Word;
    Name: string[40];
    Desc: string[255];
    North, South, East, West, Up, Down: Word;
    Active: Boolean;
    Reserved: Byte;
  end;

  TGameObjectBin = packed record
    ID: Word;
    Name: string[30];
    Desc: string[100];
    RoomID: Word;
    CarriedBy: Word;
    Flags: Byte;
    Active: Boolean;
    UseText: string[100];
    Reserved: Byte;
  end;

  TMobBin = packed record
    ID: Word;
    Name: string[30];
    Desc: string[100];
    RoomID: Word;
    Dialogue: string[200];
    Active: Boolean;
    Reserved: Byte;
  end;

  { --- Version 2 (current) --- }

  TGameHeaderV2 = packed record
    Magic: array[0..3] of Char;
    Version: Word;
    RoomCount: Word;
    ObjectCount: Word;
    MobCount: Word;
    StartRoom: Word;
    Title: string[40];
    WinRoomID: Word;
    WinObjectID: Word;
    MaxScore: Word;
    Reserved: array[0..7] of Byte;
  end;

  TRoomBinV2 = packed record
    ID: Word;
    Name: string[40];
    Desc: string[255];
    North, South, East, West, Up, Down: Word;
    Points: Word;
    Active: Boolean;
    Reserved: Byte;
  end;

  TGameObjectBinV2 = packed record
    ID: Word;
    Name: string[30];
    Desc: string[100];
    RoomID: Word;
    CarriedBy: Word;
    Flags: Byte;
    Active: Boolean;
    UseText: string[100];
    Points: Word;
    Reserved: Byte;
  end;

  TMobBinV2 = packed record
    ID: Word;
    Name: string[30];
    Desc: string[100];
    RoomID: Word;
    Dialogue: string[200];
    Active: Boolean;
    Reserved: array[0..2] of Byte;
  end;

  { --- Save game (SORS) --- }

  TSaveHeader = packed record
    Magic: array[0..3] of Char;
    Version: Word;
    WorldSig: LongWord;     { Guards against restoring into a different world }
    CurrentRoom: Word;
    Score: Word;
    Turns: Word;
    InvCount: Byte;
    Reserved: array[0..6] of Byte;
  end;

  TObjectStateRec = packed record
    ID: Word;
    RoomID: Word;
    CarriedBy: Word;
  end;

  TMobStateRec = packed record
    ID: Word;
    RoomID: Word;
  end;

function FlagsToByte(F: TObjectFlags): Byte;
begin
  Result := 0;
  if ofPickup in F then Result := Result or $01;
  if ofUse in F then Result := Result or $02;
  if ofOpen in F then Result := Result or $04;
  if ofRead in F then Result := Result or $08;
end;

function ByteToFlags(B: Byte): TObjectFlags;
begin
  Result := [];
  if (B and $01) <> 0 then Include(Result, ofPickup);
  if (B and $02) <> 0 then Include(Result, ofUse);
  if (B and $04) <> 0 then Include(Result, ofOpen);
  if (B and $08) <> 0 then Include(Result, ofRead);
end;

function DetectFileFormat(const FileName: string): TFileFormat;
var
  F: File;
  Magic: array[0..3] of Char;
  BytesRead: Integer;
  Ext: string;
begin
  Result := ffText;

  { First check file extension for BPL }
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.bpl' then
  begin
    Result := ffBPL;
    Exit;
  end;

  {$I-}
  Assign(F, FileName);
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  {$I-}
  BlockRead(F, Magic, 4, BytesRead);
  {$I+}
  Close(F);

  if (BytesRead = 4) and (Magic = SORB_MAGIC) then
    Result := ffBinary;
end;

{ Version 1 files carry no title, so fall back to the file's base name }
function TitleFromFileName(const FileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
  if Result = '' then
    Result := 'Loaded World';
end;

{ F must be open and positioned just past the version prefix }
function ReadBinaryV1(var F: File; var W: TGameWorld; const FileName: string): Boolean;
var
  Header: TGameHeader;
  RoomBin: TRoomBin;
  ObjBin: TGameObjectBin;
  MobBin: TMobBin;
  I: Integer;
  BytesRead: Integer;
begin
  Result := False;

  Seek(F, 0);
  {$I-}
  BlockRead(F, Header, SizeOf(TGameHeader), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TGameHeader)) then Exit;

  W.CurrentRoom := Header.StartRoom;
  W.Title := TitleFromFileName(FileName);

  W.RoomCount := 0;
  for I := 1 to Header.RoomCount do
  begin
    {$I-}
    BlockRead(F, RoomBin, SizeOf(TRoomBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TRoomBin)) then Exit;

    if RoomBin.Active and (W.RoomCount < MAX_ROOMS) then
    begin
      Inc(W.RoomCount);
      W.Rooms[W.RoomCount].ID := RoomBin.ID;
      W.Rooms[W.RoomCount].Name := RoomBin.Name;
      W.Rooms[W.RoomCount].Desc := RoomBin.Desc;
      W.Rooms[W.RoomCount].Exits[dirNorth] := RoomBin.North;
      W.Rooms[W.RoomCount].Exits[dirSouth] := RoomBin.South;
      W.Rooms[W.RoomCount].Exits[dirEast] := RoomBin.East;
      W.Rooms[W.RoomCount].Exits[dirWest] := RoomBin.West;
      W.Rooms[W.RoomCount].Exits[dirUp] := RoomBin.Up;
      W.Rooms[W.RoomCount].Exits[dirDown] := RoomBin.Down;
      W.Rooms[W.RoomCount].Points := 0;
      W.Rooms[W.RoomCount].Active := True;
    end;
  end;

  W.ObjectCount := 0;
  for I := 1 to Header.ObjectCount do
  begin
    {$I-}
    BlockRead(F, ObjBin, SizeOf(TGameObjectBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TGameObjectBin)) then Exit;

    if ObjBin.Active and (W.ObjectCount < MAX_OBJECTS) then
    begin
      Inc(W.ObjectCount);
      W.Objects[W.ObjectCount].ID := ObjBin.ID;
      W.Objects[W.ObjectCount].Name := ObjBin.Name;
      W.Objects[W.ObjectCount].Desc := ObjBin.Desc;
      W.Objects[W.ObjectCount].RoomID := ObjBin.RoomID;
      W.Objects[W.ObjectCount].CarriedBy := ObjBin.CarriedBy;
      W.Objects[W.ObjectCount].Flags := ByteToFlags(ObjBin.Flags);
      W.Objects[W.ObjectCount].UseText := ObjBin.UseText;
      W.Objects[W.ObjectCount].Points := 0;
      W.Objects[W.ObjectCount].Active := True;
    end;
  end;

  W.MobCount := 0;
  for I := 1 to Header.MobCount do
  begin
    {$I-}
    BlockRead(F, MobBin, SizeOf(TMobBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TMobBin)) then Exit;

    if MobBin.Active and (W.MobCount < MAX_MOBS) then
    begin
      Inc(W.MobCount);
      W.Mobs[W.MobCount].ID := MobBin.ID;
      W.Mobs[W.MobCount].Name := MobBin.Name;
      W.Mobs[W.MobCount].Desc := MobBin.Desc;
      W.Mobs[W.MobCount].RoomID := MobBin.RoomID;
      W.Mobs[W.MobCount].Dialogue := MobBin.Dialogue;
      W.Mobs[W.MobCount].Active := True;
    end;
  end;

  Result := True;
end;

function ReadBinaryV2(var F: File; var W: TGameWorld): Boolean;
var
  Header: TGameHeaderV2;
  RoomBin: TRoomBinV2;
  ObjBin: TGameObjectBinV2;
  MobBin: TMobBinV2;
  I: Integer;
  BytesRead: Integer;
begin
  Result := False;

  Seek(F, 0);
  {$I-}
  BlockRead(F, Header, SizeOf(TGameHeaderV2), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TGameHeaderV2)) then Exit;

  W.CurrentRoom := Header.StartRoom;
  W.Title := Header.Title;
  W.WinRoomID := Header.WinRoomID;
  W.WinObjectID := Header.WinObjectID;
  W.MaxScore := Header.MaxScore;

  W.RoomCount := 0;
  for I := 1 to Header.RoomCount do
  begin
    {$I-}
    BlockRead(F, RoomBin, SizeOf(TRoomBinV2), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TRoomBinV2)) then Exit;

    if RoomBin.Active and (W.RoomCount < MAX_ROOMS) then
    begin
      Inc(W.RoomCount);
      W.Rooms[W.RoomCount].ID := RoomBin.ID;
      W.Rooms[W.RoomCount].Name := RoomBin.Name;
      W.Rooms[W.RoomCount].Desc := RoomBin.Desc;
      W.Rooms[W.RoomCount].Exits[dirNorth] := RoomBin.North;
      W.Rooms[W.RoomCount].Exits[dirSouth] := RoomBin.South;
      W.Rooms[W.RoomCount].Exits[dirEast] := RoomBin.East;
      W.Rooms[W.RoomCount].Exits[dirWest] := RoomBin.West;
      W.Rooms[W.RoomCount].Exits[dirUp] := RoomBin.Up;
      W.Rooms[W.RoomCount].Exits[dirDown] := RoomBin.Down;
      W.Rooms[W.RoomCount].Points := RoomBin.Points;
      W.Rooms[W.RoomCount].Active := True;
    end;
  end;

  W.ObjectCount := 0;
  for I := 1 to Header.ObjectCount do
  begin
    {$I-}
    BlockRead(F, ObjBin, SizeOf(TGameObjectBinV2), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TGameObjectBinV2)) then Exit;

    if ObjBin.Active and (W.ObjectCount < MAX_OBJECTS) then
    begin
      Inc(W.ObjectCount);
      W.Objects[W.ObjectCount].ID := ObjBin.ID;
      W.Objects[W.ObjectCount].Name := ObjBin.Name;
      W.Objects[W.ObjectCount].Desc := ObjBin.Desc;
      W.Objects[W.ObjectCount].RoomID := ObjBin.RoomID;
      W.Objects[W.ObjectCount].CarriedBy := ObjBin.CarriedBy;
      W.Objects[W.ObjectCount].Flags := ByteToFlags(ObjBin.Flags);
      W.Objects[W.ObjectCount].UseText := ObjBin.UseText;
      W.Objects[W.ObjectCount].Points := ObjBin.Points;
      W.Objects[W.ObjectCount].Active := True;
    end;
  end;

  W.MobCount := 0;
  for I := 1 to Header.MobCount do
  begin
    {$I-}
    BlockRead(F, MobBin, SizeOf(TMobBinV2), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TMobBinV2)) then Exit;

    if MobBin.Active and (W.MobCount < MAX_MOBS) then
    begin
      Inc(W.MobCount);
      W.Mobs[W.MobCount].ID := MobBin.ID;
      W.Mobs[W.MobCount].Name := MobBin.Name;
      W.Mobs[W.MobCount].Desc := MobBin.Desc;
      W.Mobs[W.MobCount].RoomID := MobBin.RoomID;
      W.Mobs[W.MobCount].Dialogue := MobBin.Dialogue;
      W.Mobs[W.MobCount].Active := True;
    end;
  end;

  Result := True;
end;

function LoadWorldBinary(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Prefix: TVersionPrefix;
  BytesRead: Integer;
  Ok: Boolean;
begin
  Result := False;
  InitWorld(W);

  {$I-}
  Assign(F, FileName);
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  { Read only the common prefix first - a v1 file is too short to hold a v2
    header, so the layout must be chosen before reading any further }
  {$I-}
  BlockRead(F, Prefix, SizeOf(TVersionPrefix), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TVersionPrefix)) or
     (Prefix.Magic <> SORB_MAGIC) then
  begin
    Close(F);
    Exit;
  end;

  case Prefix.Version of
    1: Ok := ReadBinaryV1(F, W, FileName);
    2: Ok := ReadBinaryV2(F, W);
  else
    Ok := False;   { A newer format than this build understands }
  end;

  Close(F);
  if not Ok then Exit;

  { v1 worlds and hand-edited v2 headers may disagree with the entity data }
  if W.MaxScore = 0 then
    W.MaxScore := ComputeMaxScore(W);

  Result := W.RoomCount > 0;
end;

function SaveWorldBinary(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TGameHeaderV2;
  RoomBin: TRoomBinV2;
  ObjBin: TGameObjectBinV2;
  MobBin: TMobBinV2;
  I: Integer;
  BytesWritten: Integer;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  { Prepare header }
  Header.Magic := SORB_MAGIC;
  Header.Version := FILE_VERSION;
  Header.RoomCount := W.RoomCount;
  Header.ObjectCount := W.ObjectCount;
  Header.MobCount := W.MobCount;
  Header.StartRoom := W.CurrentRoom;
  Header.Title := W.Title;
  Header.WinRoomID := W.WinRoomID;
  Header.WinObjectID := W.WinObjectID;
  Header.MaxScore := ComputeMaxScore(W);
  FillChar(Header.Reserved, SizeOf(Header.Reserved), 0);

  { Write header }
  {$I-}
  BlockWrite(F, Header, SizeOf(TGameHeaderV2), BytesWritten);
  {$I+}
  if (IOResult <> 0) or (BytesWritten <> SizeOf(TGameHeaderV2)) then
  begin
    Close(F);
    Exit;
  end;

  { Write rooms }
  for I := 1 to MAX_ROOMS do
  begin
    if W.Rooms[I].Active then
    begin
      RoomBin.ID := W.Rooms[I].ID;
      RoomBin.Name := W.Rooms[I].Name;
      RoomBin.Desc := W.Rooms[I].Desc;
      RoomBin.North := W.Rooms[I].Exits[dirNorth];
      RoomBin.South := W.Rooms[I].Exits[dirSouth];
      RoomBin.East := W.Rooms[I].Exits[dirEast];
      RoomBin.West := W.Rooms[I].Exits[dirWest];
      RoomBin.Up := W.Rooms[I].Exits[dirUp];
      RoomBin.Down := W.Rooms[I].Exits[dirDown];
      RoomBin.Points := W.Rooms[I].Points;
      RoomBin.Active := True;
      RoomBin.Reserved := 0;

      {$I-}
      BlockWrite(F, RoomBin, SizeOf(TRoomBinV2), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TRoomBinV2)) then
      begin
        Close(F);
        Exit;
      end;
    end;
  end;

  { Write objects }
  for I := 1 to MAX_OBJECTS do
  begin
    if W.Objects[I].Active then
    begin
      ObjBin.ID := W.Objects[I].ID;
      ObjBin.Name := W.Objects[I].Name;
      ObjBin.Desc := W.Objects[I].Desc;
      ObjBin.RoomID := W.Objects[I].RoomID;
      ObjBin.CarriedBy := W.Objects[I].CarriedBy;
      ObjBin.Flags := FlagsToByte(W.Objects[I].Flags);
      ObjBin.UseText := W.Objects[I].UseText;
      ObjBin.Points := W.Objects[I].Points;
      ObjBin.Active := True;
      ObjBin.Reserved := 0;

      {$I-}
      BlockWrite(F, ObjBin, SizeOf(TGameObjectBinV2), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TGameObjectBinV2)) then
      begin
        Close(F);
        Exit;
      end;
    end;
  end;

  { Write mobs }
  for I := 1 to MAX_MOBS do
  begin
    if W.Mobs[I].Active then
    begin
      MobBin.ID := W.Mobs[I].ID;
      MobBin.Name := W.Mobs[I].Name;
      MobBin.Desc := W.Mobs[I].Desc;
      MobBin.RoomID := W.Mobs[I].RoomID;
      MobBin.Dialogue := W.Mobs[I].Dialogue;
      MobBin.Active := True;
      FillChar(MobBin.Reserved, SizeOf(MobBin.Reserved), 0);

      {$I-}
      BlockWrite(F, MobBin, SizeOf(TMobBinV2), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TMobBinV2)) then
      begin
        Close(F);
        Exit;
      end;
    end;
  end;

  Close(F);
  Result := True;
end;

function LoadWorldText(const FileName: string; var W: TGameWorld): Boolean;
var
  F: Text;
  Line, Key, Value: string;
  CurrentIdx: Integer;
  Section: TSectionType;
begin
  Result := False;
  InitWorld(W);

  {$I-}
  Assign(F, FileName);
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit;

  CurrentIdx := 0;
  Section := secNone;

  while not Eof(F) do
  begin
    ReadLn(F, Line);
    Line := Trim(Line);

    { Skip empty lines and comments }
    if (Length(Line) = 0) or (Line[1] = ';') or (Line[1] = '#') then
      Continue;

    { Check for section headers }
    if (Length(Line) > 2) and (Line[1] = '[') then
    begin
      if Pos('[WORLD]', UpperCase(Line)) = 1 then
      begin
        Section := secWorld;
      end
      else if Pos('[ROOM:', UpperCase(Line)) = 1 then
      begin
        Section := secRoom;
        { Surplus sections past the limit are skipped, not counted }
        if W.RoomCount >= MAX_ROOMS then
          CurrentIdx := 0
        else
        begin
          Inc(W.RoomCount);
          CurrentIdx := W.RoomCount;
          InitRoom(W.Rooms[CurrentIdx]);
          W.Rooms[CurrentIdx].Active := True;
          { Parse room ID from header [ROOM:n] }
          Value := Copy(Line, 7, Pos(']', Line) - 7);
          W.Rooms[CurrentIdx].ID := StrToIntDef(Value, CurrentIdx);
        end;
      end
      else if Pos('[OBJECT:', UpperCase(Line)) = 1 then
      begin
        Section := secObject;
        if W.ObjectCount >= MAX_OBJECTS then
          CurrentIdx := 0
        else
        begin
          Inc(W.ObjectCount);
          CurrentIdx := W.ObjectCount;
          InitObject(W.Objects[CurrentIdx]);
          W.Objects[CurrentIdx].Active := True;
          { Parse object ID from header [OBJECT:n] }
          Value := Copy(Line, 9, Pos(']', Line) - 9);
          W.Objects[CurrentIdx].ID := StrToIntDef(Value, CurrentIdx);
        end;
      end
      else if Pos('[MOB:', UpperCase(Line)) = 1 then
      begin
        Section := secMob;
        if W.MobCount >= MAX_MOBS then
          CurrentIdx := 0
        else
        begin
          Inc(W.MobCount);
          CurrentIdx := W.MobCount;
          InitMob(W.Mobs[CurrentIdx]);
          W.Mobs[CurrentIdx].Active := True;
          { Parse mob ID from header [MOB:n] }
          Value := Copy(Line, 6, Pos(']', Line) - 6);
          W.Mobs[CurrentIdx].ID := StrToIntDef(Value, CurrentIdx);
        end;
      end;
      Continue;
    end;

    { Parse key=value pairs }
    if ParseKeyValue(Line, Key, Value) then
    begin
      Key := UpperCase(Key);

      case Section of
        secWorld:
          begin
            if Key = 'TITLE' then
              W.Title := Value
            else if Key = 'START' then
              W.CurrentRoom := StrToIntDef(Value, 1)
            else if Key = 'WINROOM' then
              W.WinRoomID := StrToIntDef(Value, 0)
            else if Key = 'WINOBJECT' then
              W.WinObjectID := StrToIntDef(Value, 0);
          end;
        secRoom:
          if (CurrentIdx > 0) and (CurrentIdx <= MAX_ROOMS) then
          begin
            if Key = 'NAME' then
              W.Rooms[CurrentIdx].Name := Value
            else if Key = 'DESC' then
              W.Rooms[CurrentIdx].Desc := Value
            else if Key = 'NORTH' then
              W.Rooms[CurrentIdx].Exits[dirNorth] := StrToIntDef(Value, 0)
            else if Key = 'SOUTH' then
              W.Rooms[CurrentIdx].Exits[dirSouth] := StrToIntDef(Value, 0)
            else if Key = 'EAST' then
              W.Rooms[CurrentIdx].Exits[dirEast] := StrToIntDef(Value, 0)
            else if Key = 'WEST' then
              W.Rooms[CurrentIdx].Exits[dirWest] := StrToIntDef(Value, 0)
            else if Key = 'UP' then
              W.Rooms[CurrentIdx].Exits[dirUp] := StrToIntDef(Value, 0)
            else if Key = 'DOWN' then
              W.Rooms[CurrentIdx].Exits[dirDown] := StrToIntDef(Value, 0)
            else if Key = 'POINTS' then
              W.Rooms[CurrentIdx].Points := StrToIntDef(Value, 0);
          end;
        secObject:
          if (CurrentIdx > 0) and (CurrentIdx <= MAX_OBJECTS) then
          begin
            if Key = 'NAME' then
              W.Objects[CurrentIdx].Name := Value
            else if Key = 'DESC' then
              W.Objects[CurrentIdx].Desc := Value
            else if Key = 'ROOM' then
              W.Objects[CurrentIdx].RoomID := StrToIntDef(Value, 0)
            else if Key = 'CARRIEDBY' then
              W.Objects[CurrentIdx].CarriedBy := StrToIntDef(Value, 0)
            else if Key = 'FLAGS' then
              W.Objects[CurrentIdx].Flags := ParseObjectFlags(Value)
            else if Key = 'USETEXT' then
              W.Objects[CurrentIdx].UseText := Value
            else if Key = 'POINTS' then
              W.Objects[CurrentIdx].Points := StrToIntDef(Value, 0);
          end;
        secMob:
          if (CurrentIdx > 0) and (CurrentIdx <= MAX_MOBS) then
          begin
            if Key = 'NAME' then
              W.Mobs[CurrentIdx].Name := Value
            else if Key = 'DESC' then
              W.Mobs[CurrentIdx].Desc := Value
            else if Key = 'ROOM' then
              W.Mobs[CurrentIdx].RoomID := StrToIntDef(Value, 0)
            else if Key = 'DIALOGUE' then
              W.Mobs[CurrentIdx].Dialogue := Value;
          end;
      end;
    end;
  end;

  Close(F);
  W.MaxScore := ComputeMaxScore(W);
  Result := W.RoomCount > 0;
end;

function LoadWorld(const FileName: string; var W: TGameWorld): Boolean;
var
  Format: TFileFormat;
begin
  Format := DetectFileFormat(FileName);
  case Format of
    ffBinary: Result := LoadWorldBinary(FileName, W);
    ffBPL:    Result := LoadWorldBPL(FileName, W);
  else
    Result := LoadWorldText(FileName, W);
  end;
end;

function SaveWorldText(const FileName: string; var W: TGameWorld): Boolean;
var
  F: Text;
  I: Integer;
  FlagStr: string;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F);
  {$I+}
  if IOResult <> 0 then Exit;

  { Write world header }
  WriteLn(F, '; Secret Orb World Data');
  WriteLn(F, '; Generated by Secret Orb Editor');
  WriteLn(F);
  WriteLn(F, '[WORLD]');
  WriteLn(F, 'TITLE=', W.Title);
  WriteLn(F, 'START=', W.CurrentRoom);
  WriteLn(F, 'WINROOM=', W.WinRoomID);
  WriteLn(F, 'WINOBJECT=', W.WinObjectID);
  WriteLn(F);

  { Write rooms }
  for I := 1 to MAX_ROOMS do
  begin
    if W.Rooms[I].Active then
    begin
      WriteLn(F, '[ROOM:', W.Rooms[I].ID, ']');
      WriteLn(F, 'NAME=', W.Rooms[I].Name);
      WriteLn(F, 'DESC=', W.Rooms[I].Desc);
      WriteLn(F, 'NORTH=', W.Rooms[I].Exits[dirNorth]);
      WriteLn(F, 'SOUTH=', W.Rooms[I].Exits[dirSouth]);
      WriteLn(F, 'EAST=', W.Rooms[I].Exits[dirEast]);
      WriteLn(F, 'WEST=', W.Rooms[I].Exits[dirWest]);
      WriteLn(F, 'UP=', W.Rooms[I].Exits[dirUp]);
      WriteLn(F, 'DOWN=', W.Rooms[I].Exits[dirDown]);
      WriteLn(F, 'POINTS=', W.Rooms[I].Points);
      WriteLn(F);
    end;
  end;

  { Write objects }
  for I := 1 to MAX_OBJECTS do
  begin
    if W.Objects[I].Active then
    begin
      WriteLn(F, '[OBJECT:', W.Objects[I].ID, ']');
      WriteLn(F, 'NAME=', W.Objects[I].Name);
      WriteLn(F, 'DESC=', W.Objects[I].Desc);
      WriteLn(F, 'ROOM=', W.Objects[I].RoomID);
      WriteLn(F, 'CARRIEDBY=', W.Objects[I].CarriedBy);
      FlagStr := FlagsToString(W.Objects[I].Flags);
      WriteLn(F, 'FLAGS=', FlagStr);
      WriteLn(F, 'USETEXT=', W.Objects[I].UseText);
      WriteLn(F, 'POINTS=', W.Objects[I].Points);
      WriteLn(F);
    end;
  end;

  { Write mobs }
  for I := 1 to MAX_MOBS do
  begin
    if W.Mobs[I].Active then
    begin
      WriteLn(F, '[MOB:', W.Mobs[I].ID, ']');
      WriteLn(F, 'NAME=', W.Mobs[I].Name);
      WriteLn(F, 'DESC=', W.Mobs[I].Desc);
      WriteLn(F, 'ROOM=', W.Mobs[I].RoomID);
      WriteLn(F, 'DIALOGUE=', W.Mobs[I].Dialogue);
      WriteLn(F);
    end;
  end;

  Close(F);
  Result := True;
end;

function SaveWorld(const FileName: string; var W: TGameWorld): Boolean;
begin
  { Default to binary format for space savings }
  Result := SaveWorldBinary(FileName, W);
end;

function SaveWorldAs(const FileName: string; var W: TGameWorld; Format: TSaveFormat): Boolean;
begin
  case Format of
    sfBinary: Result := SaveWorldBinary(FileName, W);
    sfText:   Result := SaveWorldText(FileName, W);
    sfBPL:    Result := SaveWorldBPL(FileName, W);
  else
    Result := SaveWorldBinary(FileName, W);
  end;
end;

{ ===================== Save games (SORS) ===================== }

{ Cheap fingerprint of the world definition. Restoring a save into a different
  world would scatter objects into rooms that do not exist, so saves that do not
  match are rejected rather than applied. }
function WorldSignature(var W: TGameWorld): LongWord;
var
  I: Integer;
begin
  Result := LongWord(W.RoomCount) or (LongWord(W.ObjectCount) shl 8) or
            (LongWord(W.MobCount) shl 16);
  for I := 1 to Length(W.Title) do
    Result := ((Result shl 5) or (Result shr 27)) xor LongWord(Ord(W.Title[I]));
end;

function SaveGameState(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TSaveHeader;
  ObjState: TObjectStateRec;
  MobState: TMobStateRec;
  Visited: array[0..(MAX_ROOMS div 8) - 1] of Byte;
  Taken: array[0..(MAX_OBJECTS div 8) - 1] of Byte;
  I, BytesWritten: Integer;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  Header.Magic := SORS_MAGIC;
  Header.Version := SAVE_VERSION;
  Header.WorldSig := WorldSignature(W);
  Header.CurrentRoom := W.CurrentRoom;
  Header.Score := W.Score;
  Header.Turns := W.Turns;
  Header.InvCount := W.PlayerInvCount;
  FillChar(Header.Reserved, SizeOf(Header.Reserved), 0);

  {$I-}
  BlockWrite(F, Header, SizeOf(TSaveHeader), BytesWritten);
  BlockWrite(F, W.PlayerInventory, SizeOf(TInventory), BytesWritten);
  {$I+}
  if IOResult <> 0 then
  begin
    Close(F);
    Exit;
  end;

  { Object placement }
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      ObjState.ID := W.Objects[I].ID;
      ObjState.RoomID := W.Objects[I].RoomID;
      ObjState.CarriedBy := W.Objects[I].CarriedBy;
      {$I-}
      BlockWrite(F, ObjState, SizeOf(TObjectStateRec), BytesWritten);
      {$I+}
      if IOResult <> 0 then
      begin
        Close(F);
        Exit;
      end;
    end;

  { Mob placement }
  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
    begin
      MobState.ID := W.Mobs[I].ID;
      MobState.RoomID := W.Mobs[I].RoomID;
      {$I-}
      BlockWrite(F, MobState, SizeOf(TMobStateRec), BytesWritten);
      {$I+}
      if IOResult <> 0 then
      begin
        Close(F);
        Exit;
      end;
    end;

  { Visited rooms and already-scored objects, one bit each }
  FillChar(Visited, SizeOf(Visited), 0);
  for I := 1 to MAX_ROOMS do
    if W.Visited[I] then
      Visited[(I - 1) div 8] := Visited[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  FillChar(Taken, SizeOf(Taken), 0);
  for I := 1 to MAX_OBJECTS do
    if W.Taken[I] then
      Taken[(I - 1) div 8] := Taken[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  {$I-}
  BlockWrite(F, Visited, SizeOf(Visited), BytesWritten);
  BlockWrite(F, Taken, SizeOf(Taken), BytesWritten);
  {$I+}
  if IOResult <> 0 then
  begin
    Close(F);
    Exit;
  end;

  Close(F);
  Result := True;
end;

function LoadGameState(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TSaveHeader;
  ObjState: TObjectStateRec;
  MobState: TMobStateRec;
  Visited: array[0..(MAX_ROOMS div 8) - 1] of Byte;
  Taken: array[0..(MAX_OBJECTS div 8) - 1] of Byte;
  Inv: TInventory;
  I, Idx, BytesRead: Integer;
  ActiveObjs, ActiveMobs: Integer;
  Expected: LongInt;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  {$I-}
  BlockRead(F, Header, SizeOf(TSaveHeader), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TSaveHeader)) or
     (Header.Magic <> SORS_MAGIC) or (Header.Version <> SAVE_VERSION) or
     (Header.WorldSig <> WorldSignature(W)) or
     (Header.InvCount > MAX_INVENTORY) then
  begin
    Close(F);
    Exit;
  end;

  { The body is a fixed size for a given world, so verifying it up front means a
    truncated save is rejected outright instead of half-applied }
  ActiveObjs := 0;
  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then Inc(ActiveObjs);
  ActiveMobs := 0;
  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then Inc(ActiveMobs);

  Expected := SizeOf(TSaveHeader) + SizeOf(TInventory) +
              ActiveObjs * SizeOf(TObjectStateRec) +
              ActiveMobs * SizeOf(TMobStateRec) +
              SizeOf(Visited) + SizeOf(Taken);
  if FileSize(F) <> Expected then
  begin
    Close(F);
    Exit;
  end;

  {$I-}
  BlockRead(F, Inv, SizeOf(TInventory), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TInventory)) then
  begin
    Close(F);
    Exit;
  end;

  for I := 1 to MAX_OBJECTS do
    if W.Objects[I].Active then
    begin
      {$I-}
      BlockRead(F, ObjState, SizeOf(TObjectStateRec), BytesRead);
      {$I+}
      if (IOResult <> 0) or (BytesRead <> SizeOf(TObjectStateRec)) then
      begin
        Close(F);
        Exit;
      end;
      Idx := FindObjectByID(W, ObjState.ID);
      if Idx > 0 then
      begin
        W.Objects[Idx].RoomID := ObjState.RoomID;
        W.Objects[Idx].CarriedBy := ObjState.CarriedBy;
      end;
    end;

  for I := 1 to MAX_MOBS do
    if W.Mobs[I].Active then
    begin
      {$I-}
      BlockRead(F, MobState, SizeOf(TMobStateRec), BytesRead);
      {$I+}
      if (IOResult <> 0) or (BytesRead <> SizeOf(TMobStateRec)) then
      begin
        Close(F);
        Exit;
      end;
      Idx := FindMobByID(W, MobState.ID);
      if Idx > 0 then
        W.Mobs[Idx].RoomID := MobState.RoomID;
    end;

  {$I-}
  BlockRead(F, Visited, SizeOf(Visited), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(Visited)) then
  begin
    Close(F);
    Exit;
  end;

  {$I-}
  BlockRead(F, Taken, SizeOf(Taken), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(Taken)) then
  begin
    Close(F);
    Exit;
  end;

  for I := 1 to MAX_ROOMS do
    W.Visited[I] := (Visited[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
  for I := 1 to MAX_OBJECTS do
    W.Taken[I] := (Taken[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;

  W.PlayerInventory := Inv;
  W.PlayerInvCount := Header.InvCount;
  W.CurrentRoom := Header.CurrentRoom;
  W.Score := Header.Score;
  W.Turns := Header.Turns;

  Close(F);
  Result := True;
end;

end.
