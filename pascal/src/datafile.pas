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

function LoadWorld(const FileName: string; var W: TGameWorld): Boolean;
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
              W.Rooms[CurrentIdx].Exits[dirWest] := StrToIntDef(Value, 0);
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

function SaveWorld(const FileName: string; var W: TGameWorld): Boolean;
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

end.
