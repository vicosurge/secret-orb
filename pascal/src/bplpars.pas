{ bplpars.pas - BPL (Bracket Programming Language) parser for Secret Orb }
{ Parses .bpl files and populates TGameWorld structures }
unit BPLPars;

{$MODE OBJFPC}
{$H+}

interface

uses
  SysUtils, GameData;

const
  BPL_REVISION = 4;   { Older revisions still load; new tags default to 0 }
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

{ START is overloaded: it opens a block, as in START:ROOM, and inside a WORLD
  block it also names the starting room, as in START:R1. Only the block-type
  spelling opens a block. }
function IsBlockType(const S: string): Boolean;
begin
  Result := (S = 'WORLD') or (S = 'ROOM') or (S = 'OBJECT') or (S = 'MOB') or
            (S = 'PARAGRAPH') or (S = 'EVENT');
end;

{ A BPL tag value is one line and cannot contain braces, so paragraph line
  breaks travel as a backslash-n escape. }
function EncodeParaText(const S: TParaText): TParaText;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    case S[I] of
      #13:
        begin
          Result := Result + '\n';
          if (I < Length(S)) and (S[I + 1] = #10) then Inc(I);
        end;
      #10: Result := Result + '\n';
      '\': Result := Result + '\\';
      '{': Result := Result + '(';
      '}': Result := Result + ')';
    else
      Result := Result + S[I];
    end;
    Inc(I);
  end;
end;

{ An event message is a single line, so it needs none of the newline escaping
  above - but a brace in it would still end the tag early. Substituted the same
  way EncodeParaText does it, rather than escaped, because the reader has no
  unescaping step for ordinary tag values. }
function EscapeBraces(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    case Result[I] of
      '{': Result[I] := '(';
      '}': Result[I] := ')';
    end;
end;

function DecodeParaText(const S: TParaText): TParaText;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = '\') and (I < Length(S)) then
    begin
      Inc(I);
      if S[I] = 'n' then
        Result := Result + #13#10
      else
        Result := Result + S[I];
    end
    else
      Result := Result + S[I];
    Inc(I);
  end;
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

{ Event tag values are comma-separated lists. A BPL tag value is one line and
  cannot contain braces, but commas are fine, so the last field of an ACTION -
  the message text - takes whatever follows the third comma verbatim. }
function BPLField(var S: string): string;
var
  P: Integer;
begin
  P := Pos(',', S);
  if P = 0 then
  begin
    Result := TrimStr(S);
    S := '';
  end
  else
  begin
    Result := TrimStr(Copy(S, 1, P - 1));
    S := Copy(S, P + 1, Length(S));
  end;
end;

{ COND tag value: type,targetid,value,negate }
procedure ParseBPLCondition(const Value: string; var C: TCondition);
var
  Rest: string;
begin
  Rest := Value;
  C.CondType := ConditionFromName(BPLField(Rest));
  C.TargetID := StrToIntDef(BPLField(Rest), 0);
  C.Value := StrToIntDef(BPLField(Rest), 0);
  C.Negate := StrToIntDef(BPLField(Rest), 0) <> 0;
end;

{ ACTION tag value: type,targetid,value and optionally text }
procedure ParseBPLAction(const Value: string; var A: TAction);
var
  Rest: string;
begin
  Rest := Value;
  A.ActionType := ActionFromName(BPLField(Rest));
  A.TargetID := StrToIntDef(BPLField(Rest), 0);
  A.Value := StrToIntDef(BPLField(Rest), 0);
  A.Text := Copy(Rest, 1, MAX_EVENT_TEXT);
end;

{ FLAGNAME and COUNTERNAME tag values: number,name }
procedure SetVarName(var W: TGameWorld; const Value: string; IsFlag: Boolean);
var
  Rest, Nm: string;
  Num: Integer;
begin
  Rest := Value;
  Num := StrToIntDef(BPLField(Rest), 0);
  Nm := Copy(TrimStr(Rest), 1, MAX_VAR_NAME);
  if IsFlag then
  begin
    if (Num >= 1) and (Num <= MAX_FLAGS) then W.FlagNames[Num] := Nm;
  end
  else
    if (Num >= 1) and (Num <= MAX_COUNTERS) then W.CounterNames[Num] := Nm;
end;

{ Raw reference text exactly as written in the file, held between the two
  passes. A reference is either a numeric ID or a VAR name, and a VAR may be
  used before the block that defines it, so none of these can be resolved while
  the first pass is still running. Unit-level rather than local because this is
  roughly 20KB - too much to put on the DOS stack. }
var
  ExitRefs: array[1..MAX_ROOMS] of array[TDirection] of string[10];
  ObjRoomRefs: array[1..MAX_OBJECTS] of string[10];
  ObjCarriedRefs: array[1..MAX_OBJECTS] of string[10];
  MobRoomRefs: array[1..MAX_MOBS] of string[10];

procedure ClearRefs;
var
  I: Integer;
  D: TDirection;
begin
  for I := 1 to MAX_ROOMS do
    for D := Low(TDirection) to High(TDirection) do
      ExitRefs[I][D] := '';
  for I := 1 to MAX_OBJECTS do
  begin
    ObjRoomRefs[I] := '';
    ObjCarriedRefs[I] := '';
  end;
  for I := 1 to MAX_MOBS do
    MobRoomRefs[I] := '';
end;

{ Resolves one stored reference. A literal number is taken as an ID; anything
  else is looked up in the symbol table. A name that resolves to nothing is
  reported - it used to become 0 silently, which produced a room with a missing
  exit and no indication of why. }
function ResolveRef(var P: TBPLParser; const Ref, Context: string): Word;
var
  I: Integer;
  AllDigits: Boolean;
begin
  Result := 0;
  if (Ref = '') or (Ref = '0') then Exit;

  AllDigits := True;
  for I := 1 to Length(Ref) do
    if (Ref[I] < '0') or (Ref[I] > '9') then
    begin
      AllDigits := False;
      Break;
    end;

  if AllDigits then
  begin
    Result := StrToIntDef(Ref, 0);
    Exit;
  end;

  Result := ResolveVAR(P, Ref);
  if Result = 0 then
    AddError(P, beBrokenReference,
             'Undefined VAR "' + Ref + '" referenced by ' + Context);
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
  TempPoints: Word;
  TempExits: array[TDirection] of string;
  TempFirstVisit, TempFirstTake, TempFirstTalk: Word;
  TempText: TParaText;
  TempEvent: TWorldEvent;
  Dir: TDirection;
begin
  Result := False;
  InitParser(GlobalParser);
  InitWorld(W);
  ClearRefs;

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
      if (Tags[I].Key = 'START') and
         ((not InBlock) or IsBlockType(UpperStr(Tags[I].Value))) then
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
          TempPoints := 0;
          TempFirstVisit := 0;
          TempFirstTake := 0;
          TempFirstTalk := 0;
          TempText := '';
          InitEvent(TempEvent);
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
              { Bound first, then increment. Incrementing first left RoomCount
                past the end of the array on overflow, and every later loop
                over 1..RoomCount - including the save path - read garbage. }
              if W.RoomCount >= MAX_ROOMS then
                AddError(GlobalParser, beValueTooLong,
                         'More than ' + IntToStr(MAX_ROOMS) + ' rooms')
              else
              begin
                Inc(W.RoomCount);
                CurrentRoom := W.RoomCount;
                W.Rooms[CurrentRoom].ID := TempOC;
                W.Rooms[CurrentRoom].Name := TempName;
                W.Rooms[CurrentRoom].Desc := TempDesc;
                W.Rooms[CurrentRoom].Points := TempPoints;
                W.Rooms[CurrentRoom].FirstVisitPara := TempFirstVisit;
                W.Rooms[CurrentRoom].Active := True;
                { Exits are kept as written and resolved in the second pass }
                for Dir := Low(TDirection) to High(TDirection) do
                begin
                  ExitRefs[CurrentRoom][Dir] := TempExits[Dir];
                  W.Rooms[CurrentRoom].Exits[Dir] := DIR_NONE;
                end;
                RegisterVAR(GlobalParser, TempVAR, 'R', TempOC);
              end;
            end;
          end
          else if BlockType = 'OBJECT' then
          begin
            if TempVAR <> '' then
            begin
              if W.ObjectCount >= MAX_OBJECTS then
                AddError(GlobalParser, beValueTooLong,
                         'More than ' + IntToStr(MAX_OBJECTS) + ' objects')
              else
              begin
                Inc(W.ObjectCount);
                CurrentObject := W.ObjectCount;
                W.Objects[CurrentObject].ID := TempOC;
                W.Objects[CurrentObject].Name := TempName;
                W.Objects[CurrentObject].Desc := TempDesc;
                ObjRoomRefs[CurrentObject] := TempRoomID;
                ObjCarriedRefs[CurrentObject] := TempCarriedBy;
                W.Objects[CurrentObject].RoomID := 0;
                W.Objects[CurrentObject].CarriedBy := 0;
                W.Objects[CurrentObject].Flags := TempFlags;
                W.Objects[CurrentObject].UseText := TempUseText;
                W.Objects[CurrentObject].Points := TempPoints;
                W.Objects[CurrentObject].FirstTakePara := TempFirstTake;
                W.Objects[CurrentObject].Active := True;
                RegisterVAR(GlobalParser, TempVAR, 'O', TempOC);
              end;
            end;
          end
          else if BlockType = 'MOB' then
          begin
            if TempVAR <> '' then
            begin
              if W.MobCount >= MAX_MOBS then
                AddError(GlobalParser, beValueTooLong,
                         'More than ' + IntToStr(MAX_MOBS) + ' mobs')
              else
              begin
                Inc(W.MobCount);
                CurrentMob := W.MobCount;
                W.Mobs[CurrentMob].ID := TempOC;
                W.Mobs[CurrentMob].Name := TempName;
                W.Mobs[CurrentMob].Desc := TempDesc;
                MobRoomRefs[CurrentMob] := TempRoomID;
                W.Mobs[CurrentMob].RoomID := 0;
                W.Mobs[CurrentMob].Dialogue := TempDialogue;
                W.Mobs[CurrentMob].FirstTalkPara := TempFirstTalk;
                W.Mobs[CurrentMob].Active := True;
                RegisterVAR(GlobalParser, TempVAR, 'M', TempOC);
              end;
            end;
          end
          else if BlockType = 'PARAGRAPH' then
          begin
            { Paragraphs are keyed by plain number rather than a VAR, so the
              number an author prints in the booklet is the number they wrote }
            if (TempOC >= 1) and (TempOC <= MAX_PARAGRAPHS) then
              SetParagraph(W, TempOC, DecodeParaText(TempText));
          end
          else if BlockType = 'EVENT' then
          begin
            { Keyed by an OC number like a paragraph rather than a VAR, and for a
              stronger reason: the number is the slot, and a save game's Fired
              bitmap is indexed by it. Events refer to rooms and objects by ID
              and nothing refers back, so the VAR pass has nothing to fix up. }
            if (TempOC >= 1) and (TempOC <= MAX_EVENTS) then
            begin
              W.Events[TempOC] := TempEvent;
              W.Events[TempOC].Name := TempName;
              W.Events[TempOC].Active := True;
              if TempOC > W.EventCount then W.EventCount := TempOC;
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
        else if Tags[I].Key = 'WINROOM' then
          W.WinRoomID := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'WINOBJ' then
          W.WinObjectID := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'INTRO' then
          W.IntroPara := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'WINPARA' then
          W.WinPara := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'LOSEPARA' then
          W.LosePara := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'BOOKLET' then
        begin
          if StrToIntDef(Tags[I].Value, 0) <> 0 then
            W.WorldFlags := W.WorldFlags or WF_BOOKLET
          else
            W.WorldFlags := W.WorldFlags and not WF_BOOKLET;
        end
        else if Tags[I].Key = 'FIRSTVISIT' then
          TempFirstVisit := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'FIRSTTAKE' then
          TempFirstTake := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'FIRSTTALK' then
          TempFirstTalk := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'TEXT' then
          TempText := Tags[I].Value
        else if Tags[I].Key = 'POINTS' then
          TempPoints := StrToIntDef(Tags[I].Value, 0)
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
          TempExits[dirDown] := Tags[I].Value
        { Event tags. COND and ACTION repeat and are kept in the order met. }
        else if Tags[I].Key = 'TRIGGER' then
          TempEvent.TriggerType := TriggerFromName(Tags[I].Value)
        else if Tags[I].Key = 'TRIGGERID' then
          TempEvent.TriggerID := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'TRIGGERID2' then
          TempEvent.TriggerID2 := StrToIntDef(Tags[I].Value, 0)
        else if Tags[I].Key = 'ONESHOT' then
          TempEvent.OneShot := StrToIntDef(Tags[I].Value, 1) <> 0
        else if Tags[I].Key = 'ENABLED' then
          TempEvent.Enabled := StrToIntDef(Tags[I].Value, 1) <> 0
        else if Tags[I].Key = 'COND' then
        begin
          if TempEvent.CondCount < MAX_CONDITIONS then
          begin
            Inc(TempEvent.CondCount);
            ParseBPLCondition(Tags[I].Value,
                              TempEvent.Conditions[TempEvent.CondCount]);
          end;
        end
        else if Tags[I].Key = 'ACTION' then
        begin
          if TempEvent.ActionCount < MAX_ACTIONS then
          begin
            Inc(TempEvent.ActionCount);
            ParseBPLAction(Tags[I].Value,
                           TempEvent.Actions[TempEvent.ActionCount]);
          end;
        end
        else if Tags[I].Key = 'FLAGNAME' then
          SetVarName(W, Tags[I].Value, True)
        else if Tags[I].Key = 'COUNTERNAME' then
          SetVarName(W, Tags[I].Value, False);
      end;
    end;

    FullLine := '';
  end;

  Close(F);

  { Second pass: resolve the references held from the first pass. It has to be
    a separate pass because a VAR may be used before the block defining it.
    Line 0 rather than the last line read: an error found here belongs to no
    particular line, and reporting EOF would send an author to the wrong place. }
  GlobalParser.CurrentLine := 0;
  for I := 1 to W.RoomCount do
    if W.Rooms[I].Active then
      for Dir := Low(TDirection) to High(TDirection) do
        W.Rooms[I].Exits[Dir] :=
          ResolveRef(GlobalParser, ExitRefs[I][Dir],
                     'the ' + GetExitName(Dir) + ' exit of room ' +
                     IntToStr(W.Rooms[I].ID));

  for I := 1 to W.ObjectCount do
    if W.Objects[I].Active then
    begin
      W.Objects[I].RoomID :=
        ResolveRef(GlobalParser, ObjRoomRefs[I],
                   'the room of object ' + IntToStr(W.Objects[I].ID));
      W.Objects[I].CarriedBy :=
        ResolveRef(GlobalParser, ObjCarriedRefs[I],
                   'the carrier of object ' + IntToStr(W.Objects[I].ID));
    end;

  for I := 1 to W.MobCount do
    if W.Mobs[I].Active then
      W.Mobs[I].RoomID :=
        ResolveRef(GlobalParser, MobRoomRefs[I],
                   'the room of mob ' + IntToStr(W.Mobs[I].ID));

  W.MaxScore := ComputeMaxScore(W);
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
  I, J: Integer;
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
  if (W.WinRoomID > 0) or (W.WinObjectID > 0) then
    WriteLn(F, '{WINROOM:', W.WinRoomID, '}{WINOBJ:', W.WinObjectID, '}');
  if (W.IntroPara > 0) or (W.WinPara > 0) or (W.LosePara > 0) then
    WriteLn(F, '{INTRO:', W.IntroPara, '}{WINPARA:', W.WinPara,
            '}{LOSEPARA:', W.LosePara, '}');
  if (W.WorldFlags and WF_BOOKLET) <> 0 then
    WriteLn(F, '{BOOKLET:1}');
  for J := 1 to MAX_FLAGS do
    if W.FlagNames[J] <> '' then
      WriteLn(F, '{FLAGNAME:', J, ',', W.FlagNames[J], '}');
  for J := 1 to MAX_COUNTERS do
    if W.CounterNames[J] <> '' then
      WriteLn(F, '{COUNTERNAME:', J, ',', W.CounterNames[J], '}');
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
      if W.Rooms[I].Points > 0 then
        WriteLn(F, '{POINTS:', W.Rooms[I].Points, '}');
      if W.Rooms[I].FirstVisitPara > 0 then
        WriteLn(F, '{FIRSTVISIT:', W.Rooms[I].FirstVisitPara, '}');
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
      if W.Objects[I].Points > 0 then
        WriteLn(F, '{POINTS:', W.Objects[I].Points, '}');
      if W.Objects[I].FirstTakePara > 0 then
        WriteLn(F, '{FIRSTTAKE:', W.Objects[I].FirstTakePara, '}');
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
      if W.Mobs[I].FirstTalkPara > 0 then
        WriteLn(F, '{FIRSTTALK:', W.Mobs[I].FirstTalkPara, '}');
      WriteLn(F, '{END}');
      WriteLn(F);
    end;
  end;

  { Write PARAGRAPH blocks. OC is the booklet number, so gaps are preserved. }
  for I := 1 to W.ParaCount do
  begin
    if W.Paragraphs[I] <> '' then
    begin
      WriteLn(F, '{START:PARAGRAPH}');
      WriteLn(F, '{REVISION:', BPL_REVISION, '}');
      WriteLn(F, '{OC:', I, '}');
      WriteLn(F, '{TEXT:', EncodeParaText(W.Paragraphs[I]), '}');
      WriteLn(F, '{END}');
      WriteLn(F);
    end;
  end;

  { Write EVENT blocks. Keyed by OC like a paragraph: an event refers to rooms
    and objects by ID and nothing refers back to it, so it needs no VAR. }
  for I := 1 to MAX_EVENTS do
  begin
    if W.Events[I].Active then
    begin
      WriteLn(F, '{START:EVENT}');
      WriteLn(F, '{REVISION:', BPL_REVISION, '}');
      WriteLn(F, '{OC:', I, '}{NAME:', W.Events[I].Name, '}+++');
      Write(F, '{TRIGGER:', TriggerName(W.Events[I].TriggerType), '}',
               '{TRIGGERID:', W.Events[I].TriggerID, '}');
      if W.Events[I].TriggerID2 > 0 then
        Write(F, '{TRIGGERID2:', W.Events[I].TriggerID2, '}');
      WriteLn(F, '{ONESHOT:', Ord(W.Events[I].OneShot), '}',
                 '{ENABLED:', Ord(W.Events[I].Enabled), '}');
      for J := 1 to W.Events[I].CondCount do
        with W.Events[I].Conditions[J] do
          WriteLn(F, '{COND:', ConditionName(CondType), ',', TargetID, ',',
                  Value, ',', Ord(Negate), '}');
      for J := 1 to W.Events[I].ActionCount do
        with W.Events[I].Actions[J] do
          WriteLn(F, '{ACTION:', ActionName(ActionType), ',', TargetID, ',',
                  Value, ',', EscapeBraces(Text), '}');
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
