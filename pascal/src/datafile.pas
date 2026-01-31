{ datafile.pas - Text-based data file I/O for Secret Orb }
unit DataFile;

{$MODE OBJFPC}

interface

uses
  SysUtils, GameData;

function LoadWorld(const FileName: string; var W: TGameWorld): Boolean;
function SaveWorld(const FileName: string; var W: TGameWorld): Boolean;
function FindRoomByID(var W: TGameWorld; ID: Word): Integer;

implementation

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
  TFileFormat = (ffText, ffBinary);

const
  SORB_MAGIC = 'SORB';
  FILE_VERSION = 1;

type
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
begin
  Result := ffText;

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

function LoadWorldBinary(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TGameHeader;
  RoomBin: TRoomBin;
  ObjBin: TGameObjectBin;
  MobBin: TMobBin;
  I, RoomIdx: Integer;
  BytesRead: Integer;
begin
  Result := False;
  InitWorld(W);

  {$I-}
  Assign(F, FileName);
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  { Read and validate header }
  {$I-}
  BlockRead(F, Header, SizeOf(TGameHeader), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TGameHeader)) then
  begin
    Close(F);
    Exit;
  end;

  if (Header.Magic <> SORB_MAGIC) or (Header.Version <> FILE_VERSION) then
  begin
    Close(F);
    Exit;
  end;

  { Set world properties }
  W.CurrentRoom := Header.StartRoom;
  W.Title := 'Loaded World';

  { Read rooms }
  W.RoomCount := 0;
  for I := 1 to Header.RoomCount do
  begin
    {$I-}
    BlockRead(F, RoomBin, SizeOf(TRoomBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TRoomBin)) then
    begin
      Close(F);
      Exit;
    end;

    if RoomBin.Active and (W.RoomCount < MAX_ROOMS) then
    begin
      Inc(W.RoomCount);
      RoomIdx := W.RoomCount;
      W.Rooms[RoomIdx].ID := RoomBin.ID;
      W.Rooms[RoomIdx].Name := RoomBin.Name;
      W.Rooms[RoomIdx].Desc := RoomBin.Desc;
      W.Rooms[RoomIdx].Exits[dirNorth] := RoomBin.North;
      W.Rooms[RoomIdx].Exits[dirSouth] := RoomBin.South;
      W.Rooms[RoomIdx].Exits[dirEast] := RoomBin.East;
      W.Rooms[RoomIdx].Exits[dirWest] := RoomBin.West;
      W.Rooms[RoomIdx].Exits[dirUp] := RoomBin.Up;
      W.Rooms[RoomIdx].Exits[dirDown] := RoomBin.Down;
      W.Rooms[RoomIdx].Active := True;
    end;
  end;

  { Read objects }
  W.ObjectCount := 0;
  for I := 1 to Header.ObjectCount do
  begin
    {$I-}
    BlockRead(F, ObjBin, SizeOf(TGameObjectBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TGameObjectBin)) then
    begin
      Close(F);
      Exit;
    end;

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
      W.Objects[W.ObjectCount].Active := True;
    end;
  end;

  { Read mobs }
  W.MobCount := 0;
  for I := 1 to Header.MobCount do
  begin
    {$I-}
    BlockRead(F, MobBin, SizeOf(TMobBin), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(TMobBin)) then
    begin
      Close(F);
      Exit;
    end;

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

  Close(F);
  Result := W.RoomCount > 0;
end;

function SaveWorldBinary(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TGameHeader;
  RoomBin: TRoomBin;
  ObjBin: TGameObjectBin;
  MobBin: TMobBin;
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
  FillChar(Header.Reserved, SizeOf(Header.Reserved), 0);

  { Write header }
  {$I-}
  BlockWrite(F, Header, SizeOf(TGameHeader), BytesWritten);
  {$I+}
  if (IOResult <> 0) or (BytesWritten <> SizeOf(TGameHeader)) then
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
      RoomBin.Active := True;
      RoomBin.Reserved := 0;

      {$I-}
      BlockWrite(F, RoomBin, SizeOf(TRoomBin), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TRoomBin)) then
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
      ObjBin.Active := True;
      ObjBin.Reserved := 0;

      {$I-}
      BlockWrite(F, ObjBin, SizeOf(TGameObjectBin), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TGameObjectBin)) then
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
      MobBin.Reserved := 0;

      {$I-}
      BlockWrite(F, MobBin, SizeOf(TMobBin), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TMobBin)) then
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
        Inc(W.RoomCount);
        CurrentIdx := W.RoomCount;
        if CurrentIdx <= MAX_ROOMS then
        begin
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
        Inc(W.ObjectCount);
        CurrentIdx := W.ObjectCount;
        if CurrentIdx <= MAX_OBJECTS then
        begin
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
        Inc(W.MobCount);
        CurrentIdx := W.MobCount;
        if CurrentIdx <= MAX_MOBS then
        begin
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
              W.CurrentRoom := StrToIntDef(Value, 1);
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
              W.Rooms[CurrentIdx].Exits[dirDown] := StrToIntDef(Value, 0);
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
              W.Objects[CurrentIdx].UseText := Value;
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
  Result := W.RoomCount > 0;
end;

function LoadWorld(const FileName: string; var W: TGameWorld): Boolean;
var
  Format: TFileFormat;
begin
  Format := DetectFileFormat(FileName);
  if Format = ffBinary then
    Result := LoadWorldBinary(FileName, W)
  else
    Result := LoadWorldText(FileName, W);
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

end.
