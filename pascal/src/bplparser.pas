{ bplparser.pas - BPL (Bracket Programming Language) parser for Secret Orb }
{ Parses .bpl files and populates TGameWorld structures }
unit BPLParser;

{$MODE OBJFPC}
{$H+}

interface

uses
  SysUtils, GameData;

const
  BPL_REVISION = 1;
  MAX_BPL_ERRORS = 50;

type
  TBPLErrorCode = (
    beNone,
    beUnclosedBrace,      { E001 }
    beMissingStart,       { E002 }
    beMissingEnd,         { E003 }
    beMissingRevision,    { E004 }
    beUnknownType,        { E005 }
    beMissingRequired,    { E006 }
    beInvalidVAR,         { E007 }
    beBrokenReference,    { E008 }
    beDuplicateOC,        { E009 }
    beDuplicateVAR,       { E010 }
    beValueTooLong        { E011 }
  );

  TBPLError = record
    Code: TBPLErrorCode;
    Line: Integer;
    Message: string;
  end;

  TBPLErrorArray = array[1..MAX_BPL_ERRORS] of TBPLError;

  { Symbol table entry for VAR resolution }
  TVAREntry = record
    VARName: string[10];
    EntityType: Char;     { R=Room, O=Object, M=Mob }
    ID: Word;
    Active: Boolean;
  end;

  TVARTable = array[1..512] of TVAREntry;

  { Parser state }
  TBPLParser = record
    Errors: TBPLErrorArray;
    ErrorCount: Integer;
    VARTable: TVARTable;
    VARCount: Integer;
    CurrentLine: Integer;
  end;

{ Main parsing functions }
function LoadWorldBPL(const FileName: string; var W: TGameWorld): Boolean;
function SaveWorldBPL(const FileName: string; var W: TGameWorld): Boolean;
function GetBPLErrors(var Errors: TBPLErrorArray; var Count: Integer): Boolean;

{ Utility functions }
function IsBPLFile(const FileName: string): Boolean;

implementation

var
  GlobalParser: TBPLParser;

{ Initialize parser state }
procedure InitParser(var P: TBPLParser);
var
  I: Integer;
begin
  P.ErrorCount := 0;
  P.VARCount := 0;
  P.CurrentLine := 0;
  for I := 1 to MAX_BPL_ERRORS do
  begin
    P.Errors[I].Code := beNone;
    P.Errors[I].Line := 0;
    P.Errors[I].Message := '';
  end;
  for I := 1 to 512 do
  begin
    P.VARTable[I].VARName := '';
    P.VARTable[I].EntityType := ' ';
    P.VARTable[I].ID := 0;
    P.VARTable[I].Active := False;
  end;
end;

{ Add error to parser }
procedure AddError(var P: TBPLParser; Code: TBPLErrorCode; const Msg: string);
begin
  if P.ErrorCount < MAX_BPL_ERRORS then
  begin
    Inc(P.ErrorCount);
    P.Errors[P.ErrorCount].Code := Code;
    P.Errors[P.ErrorCount].Line := P.CurrentLine;
    P.Errors[P.ErrorCount].Message := Msg;
  end;
end;

{ Register a VAR in the symbol table }
function RegisterVAR(var P: TBPLParser; const VARName: string; EntityType: Char; ID: Word): Boolean;
var
  I: Integer;
begin
  Result := False;

  { Check for duplicates }
  for I := 1 to P.VARCount do
    if P.VARTable[I].Active and (P.VARTable[I].VARName = VARName) then
    begin
      AddError(P, beDuplicateVAR, 'Duplicate VAR: ' + VARName);
      Exit;
    end;

  { Add new entry }
  if P.VARCount < 512 then
  begin
    Inc(P.VARCount);
    P.VARTable[P.VARCount].VARName := VARName;
    P.VARTable[P.VARCount].EntityType := EntityType;
    P.VARTable[P.VARCount].ID := ID;
    P.VARTable[P.VARCount].Active := True;
    Result := True;
  end;
end;

{ Resolve a VAR to its numeric ID }
function ResolveVAR(var P: TBPLParser; const VARName: string): Word;
var
  I: Integer;
begin
  Result := 0;

  { Handle '0' as no reference }
  if (VARName = '0') or (VARName = '') then
    Exit;

  for I := 1 to P.VARCount do
    if P.VARTable[I].Active and (P.VARTable[I].VARName = VARName) then
    begin
      Result := P.VARTable[I].ID;
      Exit;
    end;

  { VAR not found - will be checked in second pass }
end;

{ String utility functions }
function TrimStr(const S: string): string;
var
  I, J: Integer;
begin
  I := 1;
  J := Length(S);
  while (I <= J) and (S[I] <= ' ') do Inc(I);
  while (J >= I) and (S[J] <= ' ') do Dec(J);
  Result := Copy(S, I, J - I + 1);
end;

function UpperStr(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if (Result[I] >= 'a') and (Result[I] <= 'z') then
      Result[I] := Chr(Ord(Result[I]) - 32);
end;

{ Parse a single BPL tag {KEY:VALUE} }
function ParseTag(const Tag: string; var Key, Value: string): Boolean;
var
  P: Integer;
  Inner: string;
begin
  Result := False;
  Key := '';
  Value := '';

  { Must start with { and end with } }
  if (Length(Tag) < 3) or (Tag[1] <> '{') or (Tag[Length(Tag)] <> '}') then
    Exit;

  { Extract inner content }
  Inner := Copy(Tag, 2, Length(Tag) - 2);

  { Find colon separator }
  P := Pos(':', Inner);
  if P > 0 then
  begin
    Key := UpperStr(TrimStr(Copy(Inner, 1, P - 1)));
    Value := TrimStr(Copy(Inner, P + 1, Length(Inner) - P));
    Result := True;
  end
  else
  begin
    { Tags without value (like {END}) }
    Key := UpperStr(TrimStr(Inner));
    Value := '';
    Result := True;
  end;
end;

{ Extract all tags from a line }
type
  TTagArray = array[1..20] of record
    Key: string;
    Value: string;
  end;

function ExtractTags(const Line: string; var Tags: TTagArray; var Count: Integer): Boolean;
var
  I, Start: Integer;
  InTag: Boolean;
  Tag, Key, Value: string;
begin
  Result := True;
  Count := 0;
  InTag := False;
  Start := 0;

  for I := 1 to Length(Line) do
  begin
    if Line[I] = '{' then
    begin
      if InTag then
      begin
        Result := False; { Nested brace }
        Exit;
      end;
      InTag := True;
      Start := I;
    end
    else if Line[I] = '}' then
    begin
      if not InTag then
      begin
        Result := False; { Unmatched close brace }
        Exit;
      end;
      InTag := False;
      Tag := Copy(Line, Start, I - Start + 1);
      if ParseTag(Tag, Key, Value) and (Count < 20) then
      begin
        Inc(Count);
        Tags[Count].Key := Key;
        Tags[Count].Value := Value;
      end;
    end;
  end;

  if InTag then
    Result := False; { Unclosed brace }
end;

{ Parse object flags from comma-separated string }
function ParseBPLFlags(const S: string): TObjectFlags;
var
  Upper: string;
begin
  Result := [];
  Upper := UpperStr(S);
  if Pos('PICKUP', Upper) > 0 then Include(Result, ofPickup);
  if Pos('USE', Upper) > 0 then Include(Result, ofUse);
  if Pos('OPEN', Upper) > 0 then Include(Result, ofOpen);
  if Pos('READ', Upper) > 0 then Include(Result, ofRead);
end;

{ Main BPL loading function }
function LoadWorldBPL(const FileName: string; var W: TGameWorld): Boolean;
var
  F: Text;
  Line, FullLine: string;
  Tags: TTagArray;
  TagCount, I: Integer;
  InBlock: Boolean;
  BlockType: string;
  CurrentRoom: Integer;
  CurrentObject: Integer;
  CurrentMob: Integer;
  HasRevision: Boolean;
  TempOC: Word;
  TempVAR, TempName, TempDesc: string;
  TempRoomID, TempCarriedBy: string;
  TempFlags: TObjectFlags;
  TempUseText, TempDialogue: string;
  TempExits: array[TDirection] of string;
  Dir: TDirection;
begin
  Result := False;
  InitParser(GlobalParser);
  InitWorld(W);

  {$I-}
  Assign(F, FileName);
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit;

  InBlock := False;
  BlockType := '';
  CurrentRoom := 0;
  CurrentObject := 0;
  CurrentMob := 0;
  FullLine := '';

  { First pass: Build symbol table and parse all objects }
  while not Eof(F) do
  begin
    Inc(GlobalParser.CurrentLine);
    ReadLn(F, Line);
    Line := TrimStr(Line);

    { Skip empty lines and comments }
    if (Length(Line) = 0) or (Line[1] = '#') then
      Continue;

    { Handle line continuation }
    if (Length(FullLine) > 0) and (FullLine[Length(FullLine)] = '+') and
       (Length(FullLine) > 2) and (FullLine[Length(FullLine)-1] = '+') and
       (FullLine[Length(FullLine)-2] = '+') then
    begin
      { Remove +++ and append new line }
      FullLine := Copy(FullLine, 1, Length(FullLine) - 3) + Line;
    end
    else if Length(FullLine) > 0 then
    begin
      { Process previous complete line }
      { (handled below) }
      FullLine := Line;
    end
    else
      FullLine := Line;

    { Check for continuation marker }
    if (Length(FullLine) >= 3) and
       (FullLine[Length(FullLine)] = '+') and
       (FullLine[Length(FullLine)-1] = '+') and
       (FullLine[Length(FullLine)-2] = '+') then
      Continue; { Wait for more lines }

    { Extract tags from complete line }
    if not ExtractTags(FullLine, Tags, TagCount) then
    begin
      AddError(GlobalParser, beUnclosedBrace, 'Syntax error in line');
      FullLine := '';
      Continue;
    end;

    { Process tags }
    for I := 1 to TagCount do
    begin
      if Tags[I].Key = 'START' then
      begin
        if InBlock then
          AddError(GlobalParser, beMissingEnd, 'Missing END before new START')
        else
        begin
          InBlock := True;
          BlockType := UpperStr(Tags[I].Value);
          HasRevision := False;
          TempOC := 0;
          TempVAR := '';
          TempName := '';
          TempDesc := '';
          TempRoomID := '0';
          TempCarriedBy := '0';
          TempFlags := [];
          TempUseText := '';
          TempDialogue := '';
          for Dir := Low(TDirection) to High(TDirection) do
            TempExits[Dir] := '0';
        end;
      end
      else if Tags[I].Key = 'END' then
      begin
        if not InBlock then
          AddError(GlobalParser, beMissingStart, 'END without START')
        else
        begin
          { Finalize the current block }
          if BlockType = 'WORLD' then
          begin
            { World block - already processed inline }
          end
          else if BlockType = 'ROOM' then
          begin
            if TempVAR <> '' then
            begin
              Inc(W.RoomCount);
              CurrentRoom := W.RoomCount;
              if CurrentRoom <= MAX_ROOMS then
              begin
                W.Rooms[CurrentRoom].ID := TempOC;
                W.Rooms[CurrentRoom].Name := TempName;
                W.Rooms[CurrentRoom].Desc := TempDesc;
                W.Rooms[CurrentRoom].Active := True;
                { Store exit VARs temporarily - resolve in second pass }
                for Dir := Low(TDirection) to High(TDirection) do
                  W.Rooms[CurrentRoom].Exits[Dir] := StrToIntDef(TempExits[Dir], 0);
                RegisterVAR(GlobalParser, TempVAR, 'R', TempOC);
              end;
            end;
          end
          else if BlockType = 'OBJECT' then
          begin
            if TempVAR <> '' then
            begin
              Inc(W.ObjectCount);
              CurrentObject := W.ObjectCount;
              if CurrentObject <= MAX_OBJECTS then
              begin
                W.Objects[CurrentObject].ID := TempOC;
                W.Objects[CurrentObject].Name := TempName;
                W.Objects[CurrentObject].Desc := TempDesc;
                W.Objects[CurrentObject].RoomID := StrToIntDef(TempRoomID, 0);
                W.Objects[CurrentObject].CarriedBy := StrToIntDef(TempCarriedBy, 0);
                W.Objects[CurrentObject].Flags := TempFlags;
                W.Objects[CurrentObject].UseText := TempUseText;
                W.Objects[CurrentObject].Active := True;
                RegisterVAR(GlobalParser, TempVAR, 'O', TempOC);
              end;
            end;
          end
          else if BlockType = 'MOB' then
          begin
            if TempVAR <> '' then
            begin
              Inc(W.MobCount);
              CurrentMob := W.MobCount;
              if CurrentMob <= MAX_MOBS then
              begin
                W.Mobs[CurrentMob].ID := TempOC;
                W.Mobs[CurrentMob].Name := TempName;
                W.Mobs[CurrentMob].Desc := TempDesc;
                W.Mobs[CurrentMob].RoomID := StrToIntDef(TempRoomID, 0);
                W.Mobs[CurrentMob].Dialogue := TempDialogue;
                W.Mobs[CurrentMob].Active := True;
                RegisterVAR(GlobalParser, TempVAR, 'M', TempOC);
              end;
            end;
          end;
          InBlock := False;
          BlockType := '';
        end;
      end
      else if InBlock then
      begin
        { Process block-specific tags }
        if Tags[I].Key = 'REVISION' then
          HasRevision := True
        else if Tags[I].Key = 'OC' then
          TempOC := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'VAR' then
          TempVAR := UpperStr(Tags[I].Value)
        else if Tags[I].Key = 'NAME' then
          TempName := Tags[I].Value
        else if (Tags[I].Key = 'DESC') or (Tags[I].Key = 'DESCRIPTION') then
          TempDesc := Tags[I].Value
        else if Tags[I].Key = 'TITLE' then
          W.Title := Tags[I].Value
        else if Tags[I].Key = 'START' then
        begin
          { START tag inside WORLD block = starting room }
          if BlockType = 'WORLD' then
            W.CurrentRoom := StrToIntDef(Copy(Tags[I].Value, 2, Length(Tags[I].Value)-1), 1);
        end
        else if Tags[I].Key = 'ROOM' then
          TempRoomID := Tags[I].Value
        else if Tags[I].Key = 'CARRIEDBY' then
          TempCarriedBy := Tags[I].Value
        else if Tags[I].Key = 'FLAGS' then
          TempFlags := ParseBPLFlags(Tags[I].Value)
        else if Tags[I].Key = 'USETEXT' then
          TempUseText := Tags[I].Value
        else if Tags[I].Key = 'DIALOGUE' then
          TempDialogue := Tags[I].Value
        else if Tags[I].Key = 'NORTH' then
          TempExits[dirNorth] := Tags[I].Value
        else if Tags[I].Key = 'SOUTH' then
          TempExits[dirSouth] := Tags[I].Value
        else if Tags[I].Key = 'EAST' then
          TempExits[dirEast] := Tags[I].Value
        else if Tags[I].Key = 'WEST' then
          TempExits[dirWest] := Tags[I].Value
        else if Tags[I].Key = 'UP' then
          TempExits[dirUp] := Tags[I].Value
        else if Tags[I].Key = 'DOWN' then
          TempExits[dirDown] := Tags[I].Value;
      end;
    end;

    FullLine := '';
  end;

  Close(F);

  { Second pass: Resolve VAR references }
  for I := 1 to W.RoomCount do
  begin
    if W.Rooms[I].Active then
    begin
      { Room exits are stored as VAR references during first pass }
      { For now, we assume numeric IDs were used directly }
      { A more complete implementation would store VARs and resolve here }
    end;
  end;

  { Resolve object room references }
  for I := 1 to W.ObjectCount do
  begin
    if W.Objects[I].Active then
    begin
      { RoomID is already numeric from first pass }
    end;
  end;

  { Resolve mob room references }
  for I := 1 to W.MobCount do
  begin
    if W.Mobs[I].Active then
    begin
      { RoomID is already numeric from first pass }
    end;
  end;

  Result := (W.RoomCount > 0) and (GlobalParser.ErrorCount = 0);
end;

{ Convert object flags to comma-separated string }
function FlagsToStr(Flags: TObjectFlags): string;
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

{ Save world to BPL format }
function SaveWorldBPL(const FileName: string; var W: TGameWorld): Boolean;
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

  { Write header comment }
  WriteLn(F, '# Secret Orb World Definition (BPL Format)');
  WriteLn(F, '# Generated by Secret Orb Editor');
  WriteLn(F);

  { Write WORLD block }
  WriteLn(F, '{START:WORLD}');
  WriteLn(F, '{REVISION:', BPL_REVISION, '}{TITLE:', W.Title, '}{START:R', W.CurrentRoom, '}');
  WriteLn(F, '{END}');
  WriteLn(F);

  { Write ROOM blocks }
  for I := 1 to MAX_ROOMS do
  begin
    if W.Rooms[I].Active then
    begin
      WriteLn(F, '{START:ROOM}');
      WriteLn(F, '{REVISION:', BPL_REVISION, '}');
      WriteLn(F, '{OC:', W.Rooms[I].ID, '}{VAR:R', W.Rooms[I].ID, '}{NAME:', W.Rooms[I].Name, '}+++');
      WriteLn(F, '{DESC:', W.Rooms[I].Desc, '}+++');
      WriteLn(F, '{NORTH:', W.Rooms[I].Exits[dirNorth],
              '}{SOUTH:', W.Rooms[I].Exits[dirSouth],
              '}{EAST:', W.Rooms[I].Exits[dirEast],
              '}{WEST:', W.Rooms[I].Exits[dirWest],
              '}{UP:', W.Rooms[I].Exits[dirUp],
              '}{DOWN:', W.Rooms[I].Exits[dirDown], '}');
      WriteLn(F, '{END}');
      WriteLn(F);
    end;
  end;

  { Write OBJECT blocks }
  for I := 1 to MAX_OBJECTS do
  begin
    if W.Objects[I].Active then
    begin
      WriteLn(F, '{START:OBJECT}');
      WriteLn(F, '{REVISION:', BPL_REVISION, '}');
      WriteLn(F, '{OC:', W.Objects[I].ID, '}{VAR:O', W.Objects[I].ID, '}{NAME:', W.Objects[I].Name, '}+++');
      WriteLn(F, '{DESC:', W.Objects[I].Desc, '}+++');
      Write(F, '{ROOM:', W.Objects[I].RoomID, '}');
      if W.Objects[I].CarriedBy > 0 then
        Write(F, '{CARRIEDBY:', W.Objects[I].CarriedBy, '}');
      FlagStr := FlagsToStr(W.Objects[I].Flags);
      if FlagStr <> '' then
        Write(F, '{FLAGS:', FlagStr, '}');
      WriteLn(F, '+++');
      if W.Objects[I].UseText <> '' then
        WriteLn(F, '{USETEXT:', W.Objects[I].UseText, '}');
      WriteLn(F, '{END}');
      WriteLn(F);
    end;
  end;

  { Write MOB blocks }
  for I := 1 to MAX_MOBS do
  begin
    if W.Mobs[I].Active then
    begin
      WriteLn(F, '{START:MOB}');
      WriteLn(F, '{REVISION:', BPL_REVISION, '}');
      WriteLn(F, '{OC:', W.Mobs[I].ID, '}{VAR:M', W.Mobs[I].ID, '}{NAME:', W.Mobs[I].Name, '}+++');
      WriteLn(F, '{DESC:', W.Mobs[I].Desc, '}+++');
      WriteLn(F, '{ROOM:', W.Mobs[I].RoomID, '}');
      if W.Mobs[I].Dialogue <> '' then
        WriteLn(F, '{DIALOGUE:', W.Mobs[I].Dialogue, '}');
      WriteLn(F, '{END}');
      WriteLn(F);
    end;
  end;

  Close(F);
  Result := True;
end;

{ Get errors from last parse operation }
function GetBPLErrors(var Errors: TBPLErrorArray; var Count: Integer): Boolean;
begin
  Errors := GlobalParser.Errors;
  Count := GlobalParser.ErrorCount;
  Result := Count > 0;
end;

{ Check if file has .bpl extension }
function IsBPLFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.bpl');
end;

end.
