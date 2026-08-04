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

{ Writes a paragraph as literal lines. Shared with the editors, which use it
  to lay out the printable booklet. }
procedure WriteParaBody(var F: Text; const S: TParaText);

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
  TSectionType = (secNone, secWorld, secRoom, secObject, secMob, secParagraph,
                  secEvent);
  TFileFormat = (ffText, ffBinary, ffBPL);

const
  SORB_MAGIC = 'SORB';
  SORS_MAGIC = 'SORS';      { Save games }
  FILE_VERSION = 4;         { Versions 1, 2 and 3 still load }
  SAVE_VERSION = 3;         { Versions 1 and 2 still load }

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

  { --- Version 2 --- }

  { Version 3 keeps this layout at exactly 69 bytes: the eight bytes that were
    Reserved in version 2 are now named fields. Version 2 writers zero-filled
    them, so one record can read both - but the version 3 fields are still
    cleared explicitly when reading an older file rather than trusted. }
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
    IntroPara: Word;        { v3, offset 61 }
    WinPara: Word;          { v3, offset 63 }
    LosePara: Word;         { v3, offset 65 }
    WorldFlags: Byte;       { v3, offset 67 }
    Reserved: Byte;         { offset 68 }
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

  { --- Version 3 (current) --- }

  { Each version 3 record is its version 2 record with one Word appended, so
    the layouts stay trivially derivable - the web editor mirrors these
    offsets by hand and any drift between the two garbles every record. }

  TRoomBinV3 = packed record
    V2: TRoomBinV2;
    FirstVisitPara: Word;   { offset 315, record is 317 bytes }
  end;

  TGameObjectBinV3 = packed record
    V2: TGameObjectBinV2;
    FirstTakePara: Word;    { offset 244, record is 246 bytes }
  end;

  TMobBinV3 = packed record
    V2: TMobBinV2;
    FirstTalkPara: Word;    { offset 341, record is 343 bytes }
  end;

  { --- Version 4 (current) ---

    Version 4 is version 3 plus three trailing sections after the paragraph
    blob: events, flag names, counter names. The room, object and mob records
    are unchanged, so a v4 file is a v3 file with more on the end, and the two
    versions share a reader.

    Unlike every other record here, an event is written variable-length. A
    fixed record would have to reserve room for MAX_ACTIONS actions of
    MAX_EVENT_TEXT characters each - 764 bytes, of which a realistic event
    uses under a hundred. Each record is preceded by its own size, so a
    reader can skip a record it does not fully understand, and a truncated
    one is detectable:

      Word  EventCount              - records that follow
      repeat EventCount times:
        Word  Size                  - bytes after this field
        Word  Number                - the slot, 1..MAX_EVENTS
        Byte  NameLen ; Byte[NameLen] Name
        Byte  TriggerType           - Ord of the enum
        Word  TriggerID
        Word  TriggerID2
        Byte  Flags                 - bit 0 OneShot, bit 1 Enabled
        Byte  CondCount
        repeat CondCount times:     - 6 bytes each, only the used ones
          Byte CondType ; Word TargetID ; SmallInt Value ; Byte Negate
        Byte  ActionCount
        repeat ActionCount times:   - only the used ones
          Byte ActionType ; Word TargetID ; SmallInt Value
          Byte TextLen ; Byte[TextLen] Text

    Writing the slot number in the record is what lets events keep stable
    numbering without paying for the gaps: a deleted event costs nothing at
    all, where a deleted paragraph still costs its two-byte zero length.

    The enum fields are written as explicit Bytes rather than as the enum
    types from gamedata.pas. An FPC enum is four bytes by default, and its
    width is a compiler setting - writing the enum itself would make the file
    format depend on how the unit happened to be compiled. }

const
  EV_ONESHOT = $01;
  EV_ENABLED = $02;

type
  { Scratch for one serialised event. Ample: the largest possible event is
    about 780 bytes. Unit-level rather than on the stack, for DOS. }
  TEventBuf = array[0..1023] of Byte;

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

  { Version 3 of the save. Exits are world definition everywhere else, but
    atLockExit and atUnlockExit make them mutable, and a locked door that
    reopens itself when the player restores would be worse than no locking at
    all. 14 bytes per active room. }
  TRoomStateRec = packed record
    ID: Word;
    Exits: array[TDirection] of Word;
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

{ Reads the trailing paragraph section written by version 3. The section is
  self-describing, so a truncated or absent one leaves the world without
  paragraphs rather than failing the whole load. }
function ReadParagraphs(var F: File; var W: TGameWorld): Boolean;
var
  Count, Len, I: Word;
  BytesRead: Integer;
  Buf: array[0..MAX_PARA_LEN - 1] of Char;
  S: TParaText;
begin
  Result := False;

  {$I-}
  BlockRead(F, Count, SizeOf(Word), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(Word)) then Exit;
  if Count > MAX_PARAGRAPHS then Count := MAX_PARAGRAPHS;

  for I := 1 to Count do
  begin
    {$I-}
    BlockRead(F, Len, SizeOf(Word), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(Word)) then Exit;

    if Len = 0 then
      Continue;                { An unused slot, so numbering stays put }
    if Len > MAX_PARA_LEN then Exit;

    {$I-}
    BlockRead(F, Buf, Len, BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> Len) then Exit;

    SetLength(S, Len);
    Move(Buf, S[1], Len);
    SetParagraph(W, I, S);
  end;

  Result := True;
end;

{ --- Variable-length event serialisation ---
  Both directions walk the same buffer with a cursor, so the two stay in step
  by construction: every Put has a Get at the same point in the sequence. }

var
  EvBuf: TEventBuf;

procedure PutByte(var P: Integer; V: Byte);
begin
  if P < SizeOf(TEventBuf) then EvBuf[P] := V;
  Inc(P);
end;

procedure PutWord(var P: Integer; V: Word);
begin
  PutByte(P, V and $FF);
  PutByte(P, V shr 8);
end;

procedure PutStr(var P: Integer; const S: string);
var
  I: Integer;
begin
  PutByte(P, Length(S));
  for I := 1 to Length(S) do
    PutByte(P, Ord(S[I]));
end;

function GetByte(var P: Integer; Limit: Integer): Byte;
begin
  if P < Limit then Result := EvBuf[P] else Result := 0;
  Inc(P);
end;

function GetWord(var P: Integer; Limit: Integer): Word;
var
  Lo: Byte;
begin
  Lo := GetByte(P, Limit);
  Result := Lo or (Word(GetByte(P, Limit)) shl 8);
end;

function GetStr(var P: Integer; Limit, Max: Integer): string;
var
  Len, I: Integer;
begin
  Len := GetByte(P, Limit);
  Result := '';
  for I := 1 to Len do
    if Length(Result) < Max then
      Result := Result + Chr(GetByte(P, Limit))
    else
      GetByte(P, Limit);        { Over-long: consumed, but dropped }
end;

{ Serialises E into EvBuf, returning the length. The slot number goes in the
  record, so the writer may skip empty slots without renumbering anything. }
function EventToBuf(const E: TWorldEvent; Number: Word): Integer;
var
  P, I: Integer;
  Fl: Byte;
begin
  P := 0;
  PutWord(P, Number);
  PutStr(P, E.Name);
  PutByte(P, Ord(E.TriggerType));
  PutWord(P, E.TriggerID);
  PutWord(P, E.TriggerID2);
  Fl := 0;
  if E.OneShot then Fl := Fl or EV_ONESHOT;
  if E.Enabled then Fl := Fl or EV_ENABLED;
  PutByte(P, Fl);

  PutByte(P, E.CondCount);
  for I := 1 to E.CondCount do
    with E.Conditions[I] do
    begin
      PutByte(P, Ord(CondType));
      PutWord(P, TargetID);
      PutWord(P, Word(Value));
      PutByte(P, Ord(Negate));
    end;

  PutByte(P, E.ActionCount);
  for I := 1 to E.ActionCount do
    with E.Actions[I] do
    begin
      PutByte(P, Ord(ActionType));
      PutWord(P, TargetID);
      PutWord(P, Word(Value));
      PutStr(P, Text);
    end;

  Result := P;
end;

{ The mirror. Out-of-range enum ordinals are clamped to the inert member
  rather than cast blindly - a file written by a newer build could name an
  action this one has never heard of, and an out-of-range enum would be
  undefined behaviour on every later comparison. }
function EventFromBuf(Len: Integer; var E: TWorldEvent): Word;
var
  P, I, N: Integer;
  B, Fl: Byte;
begin
  InitEvent(E);
  P := 0;
  Result := GetWord(P, Len);
  E.Name := GetStr(P, Len, MAX_EVENT_NAME);
  B := GetByte(P, Len);
  if B <= Ord(High(TEventTrigger)) then E.TriggerType := TEventTrigger(B);
  E.TriggerID := GetWord(P, Len);
  E.TriggerID2 := GetWord(P, Len);
  Fl := GetByte(P, Len);
  E.OneShot := (Fl and EV_ONESHOT) <> 0;
  E.Enabled := (Fl and EV_ENABLED) <> 0;

  N := GetByte(P, Len);
  if N > MAX_CONDITIONS then N := MAX_CONDITIONS;
  E.CondCount := N;
  for I := 1 to N do
    with E.Conditions[I] do
    begin
      B := GetByte(P, Len);
      if B <= Ord(High(TConditionType)) then CondType := TConditionType(B);
      TargetID := GetWord(P, Len);
      Value := SmallInt(GetWord(P, Len));
      Negate := GetByte(P, Len) <> 0;
    end;

  N := GetByte(P, Len);
  if N > MAX_ACTIONS then N := MAX_ACTIONS;
  E.ActionCount := N;
  for I := 1 to N do
    with E.Actions[I] do
    begin
      B := GetByte(P, Len);
      if B <= Ord(High(TActionType)) then ActionType := TActionType(B);
      TargetID := GetWord(P, Len);
      Value := SmallInt(GetWord(P, Len));
      Text := GetStr(P, Len, MAX_EVENT_TEXT);
    end;

  E.Active := True;
end;

{ Reads the three trailing sections version 4 adds: events, then the author's
  flag and counter names. Like the paragraph section these are self-describing,
  so a version 3 file - which simply ends after the paragraphs - leaves the
  world without events rather than failing the load. }
function ReadEvents(var F: File; var W: TGameWorld): Boolean;
var
  Count, I, Slot: Word;
  Len: Byte;
  Size, BytesRead: Integer;
  Buf: array[0..MAX_VAR_NAME - 1] of Char;
  S: string;
  Ev: TWorldEvent;

  function ReadNames(Limit: Word; IsFlag: Boolean): Boolean;
  var
    N, J: Word;
  begin
    Result := False;
    {$I-}
    BlockRead(F, N, SizeOf(Word), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(Word)) then Exit;
    if N > Limit then N := Limit;
    for J := 1 to N do
    begin
      {$I-}
      BlockRead(F, Len, SizeOf(Byte), BytesRead);
      {$I+}
      if (IOResult <> 0) or (BytesRead <> SizeOf(Byte)) then Exit;
      if Len = 0 then Continue;
      if Len > MAX_VAR_NAME then Exit;
      {$I-}
      BlockRead(F, Buf, Len, BytesRead);
      {$I+}
      if (IOResult <> 0) or (BytesRead <> Len) then Exit;
      SetLength(S, Len);
      Move(Buf, S[1], Len);
      if IsFlag then W.FlagNames[J] := S else W.CounterNames[J] := S;
    end;
    Result := True;
  end;

begin
  Result := False;

  {$I-}
  BlockRead(F, Count, SizeOf(Word), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(Word)) then Exit;
  if Count > MAX_EVENTS then Exit;

  W.EventCount := 0;
  for I := 1 to Count do
  begin
    {$I-}
    BlockRead(F, Size, SizeOf(Word), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(Word)) then Exit;
    Size := Size and $FFFF;
    if (Size < 2) or (Size > SizeOf(TEventBuf)) then Exit;

    {$I-}
    BlockRead(F, EvBuf, Size, BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> Size) then Exit;

    { Any tail this build does not understand is simply not read - the cursor
      stops where the fields it knows run out, and Size has already told the
      loop where the next record begins. }
    Slot := EventFromBuf(Size, Ev);
    if (Slot >= 1) and (Slot <= MAX_EVENTS) then
    begin
      W.Events[Slot] := Ev;
      if Slot > W.EventCount then W.EventCount := Slot;
    end;
  end;

  if not ReadNames(MAX_FLAGS, True) then Exit;
  if not ReadNames(MAX_COUNTERS, False) then Exit;

  Result := True;
end;

{ Versions 2, 3 and 4 differ only by one Word appended to each record plus the
  trailing sections, so they share a reader rather than duplicating it }
function ReadBinaryV2Or3(var F: File; var W: TGameWorld; Version: Word): Boolean;
var
  Header: TGameHeaderV2;
  RoomBin: TRoomBinV3;
  ObjBin: TGameObjectBinV3;
  MobBin: TMobBinV3;
  I: Integer;
  BytesRead, RoomSize, ObjSize, MobSize: Integer;
begin
  Result := False;

  if Version >= 3 then
  begin
    RoomSize := SizeOf(TRoomBinV3);
    ObjSize := SizeOf(TGameObjectBinV3);
    MobSize := SizeOf(TMobBinV3);
  end
  else
  begin
    RoomSize := SizeOf(TRoomBinV2);
    ObjSize := SizeOf(TGameObjectBinV2);
    MobSize := SizeOf(TMobBinV2);
  end;

  Seek(F, 0);
  {$I-}
  BlockRead(F, Header, SizeOf(TGameHeaderV2), BytesRead);
  {$I+}
  if (IOResult <> 0) or (BytesRead <> SizeOf(TGameHeaderV2)) then Exit;

  { A version 2 file zero-fills these bytes, but do not take that on trust }
  if Version < 3 then
  begin
    Header.IntroPara := 0;
    Header.WinPara := 0;
    Header.LosePara := 0;
    Header.WorldFlags := 0;
  end;

  W.CurrentRoom := Header.StartRoom;
  W.Title := Header.Title;
  W.WinRoomID := Header.WinRoomID;
  W.WinObjectID := Header.WinObjectID;
  W.MaxScore := Header.MaxScore;
  W.IntroPara := Header.IntroPara;
  W.WinPara := Header.WinPara;
  W.LosePara := Header.LosePara;
  W.WorldFlags := Header.WorldFlags;

  W.RoomCount := 0;
  for I := 1 to Header.RoomCount do
  begin
    RoomBin.FirstVisitPara := 0;
    {$I-}
    BlockRead(F, RoomBin, RoomSize, BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> RoomSize) then Exit;

    if RoomBin.V2.Active and (W.RoomCount < MAX_ROOMS) then
    begin
      Inc(W.RoomCount);
      W.Rooms[W.RoomCount].ID := RoomBin.V2.ID;
      W.Rooms[W.RoomCount].Name := RoomBin.V2.Name;
      W.Rooms[W.RoomCount].Desc := RoomBin.V2.Desc;
      W.Rooms[W.RoomCount].Exits[dirNorth] := RoomBin.V2.North;
      W.Rooms[W.RoomCount].Exits[dirSouth] := RoomBin.V2.South;
      W.Rooms[W.RoomCount].Exits[dirEast] := RoomBin.V2.East;
      W.Rooms[W.RoomCount].Exits[dirWest] := RoomBin.V2.West;
      W.Rooms[W.RoomCount].Exits[dirUp] := RoomBin.V2.Up;
      W.Rooms[W.RoomCount].Exits[dirDown] := RoomBin.V2.Down;
      W.Rooms[W.RoomCount].Points := RoomBin.V2.Points;
      W.Rooms[W.RoomCount].FirstVisitPara := RoomBin.FirstVisitPara;
      W.Rooms[W.RoomCount].Active := True;
    end;
  end;

  W.ObjectCount := 0;
  for I := 1 to Header.ObjectCount do
  begin
    ObjBin.FirstTakePara := 0;
    {$I-}
    BlockRead(F, ObjBin, ObjSize, BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> ObjSize) then Exit;

    if ObjBin.V2.Active and (W.ObjectCount < MAX_OBJECTS) then
    begin
      Inc(W.ObjectCount);
      W.Objects[W.ObjectCount].ID := ObjBin.V2.ID;
      W.Objects[W.ObjectCount].Name := ObjBin.V2.Name;
      W.Objects[W.ObjectCount].Desc := ObjBin.V2.Desc;
      W.Objects[W.ObjectCount].RoomID := ObjBin.V2.RoomID;
      W.Objects[W.ObjectCount].CarriedBy := ObjBin.V2.CarriedBy;
      W.Objects[W.ObjectCount].Flags := ByteToFlags(ObjBin.V2.Flags);
      W.Objects[W.ObjectCount].UseText := ObjBin.V2.UseText;
      W.Objects[W.ObjectCount].Points := ObjBin.V2.Points;
      W.Objects[W.ObjectCount].FirstTakePara := ObjBin.FirstTakePara;
      W.Objects[W.ObjectCount].Active := True;
    end;
  end;

  W.MobCount := 0;
  for I := 1 to Header.MobCount do
  begin
    MobBin.FirstTalkPara := 0;
    {$I-}
    BlockRead(F, MobBin, MobSize, BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> MobSize) then Exit;

    if MobBin.V2.Active and (W.MobCount < MAX_MOBS) then
    begin
      Inc(W.MobCount);
      W.Mobs[W.MobCount].ID := MobBin.V2.ID;
      W.Mobs[W.MobCount].Name := MobBin.V2.Name;
      W.Mobs[W.MobCount].Desc := MobBin.V2.Desc;
      W.Mobs[W.MobCount].RoomID := MobBin.V2.RoomID;
      W.Mobs[W.MobCount].Dialogue := MobBin.V2.Dialogue;
      W.Mobs[W.MobCount].FirstTalkPara := MobBin.FirstTalkPara;
      W.Mobs[W.MobCount].Active := True;
    end;
  end;

  if Version >= 3 then
    ReadParagraphs(F, W);
  if Version >= 4 then
    ReadEvents(F, W);

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
    1:       Ok := ReadBinaryV1(F, W, FileName);
    2, 3, 4: Ok := ReadBinaryV2Or3(F, W, Prefix.Version);
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

{ The mirror of ReadEvents. Flag and counter names are written up to the
  highest one the author named, empty slots included as a zero length, so the
  numbering an author sees in the editor does not shift when they clear one. }
function WriteEvents(var F: File; var W: TGameWorld): Boolean;
var
  Count, I, Hi: Word;
  Len: Byte;
  Size, BytesWritten: Integer;
  S: string;

  function WriteNames(Limit: Word; IsFlag: Boolean): Boolean;
  var
    N, J: Word;
  begin
    Result := False;
    N := 0;
    for J := Limit downto 1 do
      if ((IsFlag) and (W.FlagNames[J] <> '')) or
         ((not IsFlag) and (W.CounterNames[J] <> '')) then
      begin
        N := J;
        Break;
      end;
    {$I-}
    BlockWrite(F, N, SizeOf(Word), BytesWritten);
    {$I+}
    if (IOResult <> 0) or (BytesWritten <> SizeOf(Word)) then Exit;
    for J := 1 to N do
    begin
      if IsFlag then S := W.FlagNames[J] else S := W.CounterNames[J];
      if Length(S) > MAX_VAR_NAME then S := Copy(S, 1, MAX_VAR_NAME);
      Len := Length(S);
      {$I-}
      BlockWrite(F, Len, SizeOf(Byte), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(Byte)) then Exit;
      if Len > 0 then
      begin
        {$I-}
        BlockWrite(F, S[1], Len, BytesWritten);
        {$I+}
        if (IOResult <> 0) or (BytesWritten <> Integer(Len)) then Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;

  Hi := W.EventCount;
  if Hi > MAX_EVENTS then Hi := MAX_EVENTS;
  Count := 0;
  for I := 1 to Hi do
    if W.Events[I].Active then Inc(Count);

  {$I-}
  BlockWrite(F, Count, SizeOf(Word), BytesWritten);
  {$I+}
  if (IOResult <> 0) or (BytesWritten <> SizeOf(Word)) then Exit;

  { Empty slots are skipped rather than written as a hole: each record carries
    its own slot number, so a gap costs nothing at all. }
  for I := 1 to Hi do
    if W.Events[I].Active then
    begin
      Size := EventToBuf(W.Events[I], I);
      {$I-}
      BlockWrite(F, Size, SizeOf(Word), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(Word)) then Exit;
      {$I-}
      BlockWrite(F, EvBuf, Size, BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> Size) then Exit;
    end;

  if not WriteNames(MAX_FLAGS, True) then Exit;
  if not WriteNames(MAX_COUNTERS, False) then Exit;

  Result := True;
end;

function SaveWorldBinary(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TGameHeaderV2;
  RoomBin: TRoomBinV3;
  ObjBin: TGameObjectBinV3;
  MobBin: TMobBinV3;
  I: Integer;
  BytesWritten: Integer;
  Count, Len: Word;
  S: TParaText;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  { Every packed record is zeroed before it is populated. A Pascal short string
    only assigns its used prefix, so without this the padding after each string
    carries whatever was in the record beforehand and BlockWrite puts that on
    disk - saving the same world twice would produce different bytes.
    Never do this to TGameWorld itself: it holds refcounted AnsiStrings. }
  FillChar(Header, SizeOf(Header), 0);

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
  Header.IntroPara := W.IntroPara;
  Header.WinPara := W.WinPara;
  Header.LosePara := W.LosePara;
  Header.WorldFlags := W.WorldFlags;
  Header.Reserved := 0;

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
      FillChar(RoomBin, SizeOf(RoomBin), 0);
      RoomBin.V2.ID := W.Rooms[I].ID;
      RoomBin.V2.Name := W.Rooms[I].Name;
      RoomBin.V2.Desc := W.Rooms[I].Desc;
      RoomBin.V2.North := W.Rooms[I].Exits[dirNorth];
      RoomBin.V2.South := W.Rooms[I].Exits[dirSouth];
      RoomBin.V2.East := W.Rooms[I].Exits[dirEast];
      RoomBin.V2.West := W.Rooms[I].Exits[dirWest];
      RoomBin.V2.Up := W.Rooms[I].Exits[dirUp];
      RoomBin.V2.Down := W.Rooms[I].Exits[dirDown];
      RoomBin.V2.Points := W.Rooms[I].Points;
      RoomBin.V2.Active := True;
      RoomBin.V2.Reserved := 0;
      RoomBin.FirstVisitPara := W.Rooms[I].FirstVisitPara;

      {$I-}
      BlockWrite(F, RoomBin, SizeOf(TRoomBinV3), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TRoomBinV3)) then
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
      FillChar(ObjBin, SizeOf(ObjBin), 0);
      ObjBin.V2.ID := W.Objects[I].ID;
      ObjBin.V2.Name := W.Objects[I].Name;
      ObjBin.V2.Desc := W.Objects[I].Desc;
      ObjBin.V2.RoomID := W.Objects[I].RoomID;
      ObjBin.V2.CarriedBy := W.Objects[I].CarriedBy;
      ObjBin.V2.Flags := FlagsToByte(W.Objects[I].Flags);
      ObjBin.V2.UseText := W.Objects[I].UseText;
      ObjBin.V2.Points := W.Objects[I].Points;
      ObjBin.V2.Active := True;
      ObjBin.V2.Reserved := 0;
      ObjBin.FirstTakePara := W.Objects[I].FirstTakePara;

      {$I-}
      BlockWrite(F, ObjBin, SizeOf(TGameObjectBinV3), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TGameObjectBinV3)) then
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
      FillChar(MobBin, SizeOf(MobBin), 0);
      MobBin.V2.ID := W.Mobs[I].ID;
      MobBin.V2.Name := W.Mobs[I].Name;
      MobBin.V2.Desc := W.Mobs[I].Desc;
      MobBin.V2.RoomID := W.Mobs[I].RoomID;
      MobBin.V2.Dialogue := W.Mobs[I].Dialogue;
      MobBin.V2.Active := True;
      MobBin.FirstTalkPara := W.Mobs[I].FirstTalkPara;

      {$I-}
      BlockWrite(F, MobBin, SizeOf(TMobBinV3), BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> SizeOf(TMobBinV3)) then
      begin
        Close(F);
        Exit;
      end;
    end;
  end;

  { Paragraph section. Every slot up to ParaCount is written, including empty
    ones as a zero length, so that paragraph numbers survive a deletion. }
  Count := W.ParaCount;
  if Count > MAX_PARAGRAPHS then Count := MAX_PARAGRAPHS;
  {$I-}
  BlockWrite(F, Count, SizeOf(Word), BytesWritten);
  {$I+}
  if (IOResult <> 0) or (BytesWritten <> SizeOf(Word)) then
  begin
    Close(F);
    Exit;
  end;

  for I := 1 to Count do
  begin
    S := W.Paragraphs[I];
    if Length(S) > MAX_PARA_LEN then
      S := Copy(S, 1, MAX_PARA_LEN);
    Len := Length(S);

    {$I-}
    BlockWrite(F, Len, SizeOf(Word), BytesWritten);
    {$I+}
    if (IOResult <> 0) or (BytesWritten <> SizeOf(Word)) then
    begin
      Close(F);
      Exit;
    end;

    if Len > 0 then
    begin
      {$I-}
      BlockWrite(F, S[1], Len, BytesWritten);
      {$I+}
      if (IOResult <> 0) or (BytesWritten <> Integer(Len)) then
      begin
        Close(F);
        Exit;
      end;
    end;
  end;

  { Event section. Unlike paragraphs, event numbers are never printed in a
    booklet, so there is nothing to keep in place: only active events are
    written, compacted, and identified by their own ID. }
  if not WriteEvents(F, W) then
  begin
    Close(F);
    Exit;
  end;

  Close(F);
  Result := True;
end;

{ Recognises only real section headers. A paragraph body is literal text and
  may well contain a line that merely starts with '[', so "begins with a
  bracket" is not a good enough test once paragraphs exist. }
function IsSectionHeader(const Line: string): Boolean;
var
  U: string;
begin
  Result := False;
  if (Length(Line) < 3) or (Line[1] <> '[') then Exit;
  U := UpperCase(Line);
  Result := (Pos('[WORLD]', U) = 1) or (Pos('[ROOM:', U) = 1) or
            (Pos('[OBJECT:', U) = 1) or (Pos('[MOB:', U) = 1) or
            (Pos('[PARAGRAPH:', U) = 1) or (Pos('[EVENT:', U) = 1);
end;

{ Splits off the text up to the next comma, advancing S past it. The event
  encodings below are comma-separated lists whose last field may itself
  contain commas, so the caller stops splitting and takes the rest verbatim. }
function NextField(var S: string): string;
var
  P: Integer;
begin
  P := Pos(',', S);
  if P = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, P - 1);
    S := Copy(S, P + 1, Length(S));
  end;
end;

{ COND=<type>,<targetid>,<value>,<negate> }
procedure ParseCondition(const Value: string; var C: TCondition);
var
  Rest: string;
begin
  Rest := Value;
  C.CondType := ConditionFromName(Trim(NextField(Rest)));
  C.TargetID := StrToIntDef(Trim(NextField(Rest)), 0);
  C.Value := StrToIntDef(Trim(NextField(Rest)), 0);
  C.Negate := StrToIntDef(Trim(NextField(Rest)), 0) <> 0;
end;

{ ACTION=<type>,<targetid>,<value>[,<text>] - the text is whatever follows the
  third comma, verbatim, so a message may contain commas of its own. }
procedure ParseAction(const Value: string; var A: TAction);
var
  Rest: string;
begin
  Rest := Value;
  A.ActionType := ActionFromName(Trim(NextField(Rest)));
  A.TargetID := StrToIntDef(Trim(NextField(Rest)), 0);
  A.Value := StrToIntDef(Trim(NextField(Rest)), 0);
  A.Text := Copy(Rest, 1, MAX_EVENT_TEXT);
end;

{ FLAG=<number>,<name> and COUNTER=<number>,<name> in [WORLD] }
procedure ParseVarName(const Value: string; var Num: Integer; var Nm: string);
var
  Rest: string;
begin
  Rest := Value;
  Num := StrToIntDef(Trim(NextField(Rest)), 0);
  Nm := Copy(Trim(Rest), 1, MAX_VAR_NAME);
end;

function LoadWorldText(const FileName: string; var W: TGameWorld): Boolean;
var
  F: Text;
  RawLine, Line, Key, Value: string;
  CurrentIdx: Integer;
  Section: TSectionType;
  ParaIdx: Integer;
  ParaBuf: TParaText;
  VarNum: Integer;
  VarName: string;

  { Paragraphs accumulate across many lines, so they are committed when the
    next section starts or the file ends }
  procedure FlushParagraph;
  var
    L: Integer;
  begin
    if ParaIdx <= 0 then Exit;
    L := Length(ParaBuf);
    while (L > 0) and ((ParaBuf[L] = #13) or (ParaBuf[L] = #10) or
                       (ParaBuf[L] = ' ')) do
      Dec(L);
    SetParagraph(W, ParaIdx, Copy(ParaBuf, 1, L));
    ParaIdx := 0;
    ParaBuf := '';
  end;

begin
  Result := False;
  InitWorld(W);
  ParaIdx := 0;
  ParaBuf := '';

  {$I-}
  Assign(F, FileName);
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit;

  CurrentIdx := 0;
  Section := secNone;

  while not Eof(F) do
  begin
    ReadLn(F, RawLine);
    Line := Trim(RawLine);

    { Inside a paragraph every line is content - blank lines are paragraph
      breaks and a leading '#' is just a character, so only a real section
      header ends the body }
    if (Section = secParagraph) and not IsSectionHeader(Line) then
    begin
      if ParaIdx > 0 then
      begin
        if ParaBuf <> '' then ParaBuf := ParaBuf + #13#10;
        ParaBuf := ParaBuf + RawLine;
      end;
      Continue;
    end;

    { Skip empty lines and comments }
    if (Length(Line) = 0) or (Line[1] = ';') or (Line[1] = '#') then
      Continue;

    { Check for section headers }
    if IsSectionHeader(Line) then
    begin
      FlushParagraph;
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
      end
      else if Pos('[EVENT:', UpperCase(Line)) = 1 then
      begin
        Section := secEvent;
        { The number in the header is the slot, not a running index - the save
          game's Fired bitmap is indexed by it, so it must not shift }
        Value := Copy(Line, 8, Pos(']', Line) - 8);
        CurrentIdx := StrToIntDef(Value, 0);
        if (CurrentIdx < 1) or (CurrentIdx > MAX_EVENTS) then
          CurrentIdx := 0
        else
        begin
          InitEvent(W.Events[CurrentIdx]);
          W.Events[CurrentIdx].Active := True;
          if CurrentIdx > W.EventCount then W.EventCount := CurrentIdx;
        end;
      end
      else if Pos('[PARAGRAPH:', UpperCase(Line)) = 1 then
      begin
        Section := secParagraph;
        { The number in the header is the paragraph number, not a running
          index - booklet numbering must survive gaps }
        Value := Copy(Line, 12, Pos(']', Line) - 12);
        ParaIdx := StrToIntDef(Value, 0);
        if (ParaIdx < 1) or (ParaIdx > MAX_PARAGRAPHS) then ParaIdx := 0;
        ParaBuf := '';
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
              W.WinObjectID := StrToIntDef(Value, 0)
            else if Key = 'INTRO' then
              W.IntroPara := StrToIntDef(Value, 0)
            else if Key = 'WINPARA' then
              W.WinPara := StrToIntDef(Value, 0)
            else if Key = 'LOSEPARA' then
              W.LosePara := StrToIntDef(Value, 0)
            else if Key = 'BOOKLET' then
            begin
              if StrToIntDef(Value, 0) <> 0 then
                W.WorldFlags := W.WorldFlags or WF_BOOKLET
              else
                W.WorldFlags := W.WorldFlags and not WF_BOOKLET;
            end
            else if Key = 'FLAG' then
            begin
              ParseVarName(Value, VarNum, VarName);
              if (VarNum >= 1) and (VarNum <= MAX_FLAGS) then
                W.FlagNames[VarNum] := VarName;
            end
            else if Key = 'COUNTER' then
            begin
              ParseVarName(Value, VarNum, VarName);
              if (VarNum >= 1) and (VarNum <= MAX_COUNTERS) then
                W.CounterNames[VarNum] := VarName;
            end;
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
              W.Rooms[CurrentIdx].Points := StrToIntDef(Value, 0)
            else if Key = 'FIRSTVISIT' then
              W.Rooms[CurrentIdx].FirstVisitPara := StrToIntDef(Value, 0);
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
              W.Objects[CurrentIdx].Points := StrToIntDef(Value, 0)
            else if Key = 'FIRSTTAKE' then
              W.Objects[CurrentIdx].FirstTakePara := StrToIntDef(Value, 0);
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
              W.Mobs[CurrentIdx].Dialogue := Value
            else if Key = 'FIRSTTALK' then
              W.Mobs[CurrentIdx].FirstTalkPara := StrToIntDef(Value, 0);
          end;
        secEvent:
          if (CurrentIdx > 0) and (CurrentIdx <= MAX_EVENTS) then
          begin
            if Key = 'NAME' then
              W.Events[CurrentIdx].Name := Value
            else if Key = 'TRIGGER' then
              W.Events[CurrentIdx].TriggerType := TriggerFromName(Value)
            else if Key = 'TRIGGERID' then
              W.Events[CurrentIdx].TriggerID := StrToIntDef(Value, 0)
            else if Key = 'TRIGGERID2' then
              W.Events[CurrentIdx].TriggerID2 := StrToIntDef(Value, 0)
            else if Key = 'ONESHOT' then
              W.Events[CurrentIdx].OneShot := StrToIntDef(Value, 1) <> 0
            else if Key = 'ENABLED' then
              W.Events[CurrentIdx].Enabled := StrToIntDef(Value, 1) <> 0
            { COND and ACTION repeat, in order, and surplus ones are dropped }
            else if Key = 'COND' then
            begin
              if W.Events[CurrentIdx].CondCount < MAX_CONDITIONS then
              begin
                Inc(W.Events[CurrentIdx].CondCount);
                ParseCondition(Value,
                  W.Events[CurrentIdx].Conditions[W.Events[CurrentIdx].CondCount]);
              end;
            end
            else if Key = 'ACTION' then
            begin
              if W.Events[CurrentIdx].ActionCount < MAX_ACTIONS then
              begin
                Inc(W.Events[CurrentIdx].ActionCount);
                ParseAction(Value,
                  W.Events[CurrentIdx].Actions[W.Events[CurrentIdx].ActionCount]);
              end;
            end;
          end;
      end;
    end;
  end;

  FlushParagraph;
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
  { All three formats can carry an event authored to start disabled, so the
    live bitmap is seeded here rather than in each loader }
  if Result then
    SeedEventState(W);
end;

procedure WriteParaBody(var F: Text; const S: TParaText);
var
  I, LineStart: Integer;
begin
  LineStart := 1;
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = #13) or (S[I] = #10) then
    begin
      WriteLn(F, Copy(S, LineStart, I - LineStart));
      if (S[I] = #13) and (I < Length(S)) and (S[I + 1] = #10) then Inc(I);
      LineStart := I + 1;
    end;
    Inc(I);
  end;
  if LineStart <= Length(S) then
    WriteLn(F, Copy(S, LineStart, Length(S) - LineStart + 1));
end;

function SaveWorldText(const FileName: string; var W: TGameWorld): Boolean;
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

  { Write world header }
  WriteLn(F, '; Secret Orb World Data');
  WriteLn(F, '; Generated by Secret Orb Editor');
  WriteLn(F);
  WriteLn(F, '[WORLD]');
  WriteLn(F, 'TITLE=', W.Title);
  WriteLn(F, 'START=', W.CurrentRoom);
  WriteLn(F, 'WINROOM=', W.WinRoomID);
  WriteLn(F, 'WINOBJECT=', W.WinObjectID);
  WriteLn(F, 'INTRO=', W.IntroPara);
  WriteLn(F, 'WINPARA=', W.WinPara);
  WriteLn(F, 'LOSEPARA=', W.LosePara);
  if (W.WorldFlags and WF_BOOKLET) <> 0 then
    WriteLn(F, 'BOOKLET=1')
  else
    WriteLn(F, 'BOOKLET=0');
  { Only the flags and counters the author actually named. The engine indexes
    them by number regardless; these lines are here so a hand-edited world
    stays readable. }
  for I := 1 to MAX_FLAGS do
    if W.FlagNames[I] <> '' then
      WriteLn(F, 'FLAG=', I, ',', W.FlagNames[I]);
  for I := 1 to MAX_COUNTERS do
    if W.CounterNames[I] <> '' then
      WriteLn(F, 'COUNTER=', I, ',', W.CounterNames[I]);
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
      WriteLn(F, 'FIRSTVISIT=', W.Rooms[I].FirstVisitPara);
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
      WriteLn(F, 'FIRSTTAKE=', W.Objects[I].FirstTakePara);
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
      WriteLn(F, 'FIRSTTALK=', W.Mobs[I].FirstTalkPara);
      WriteLn(F);
    end;
  end;

  { Write events. COND and ACTION repeat in order; the reader appends them as
    it meets them, so the order here is the order they run in. }
  for I := 1 to MAX_EVENTS do
  begin
    if W.Events[I].Active then
    begin
      WriteLn(F, '[EVENT:', I, ']');
      WriteLn(F, 'NAME=', W.Events[I].Name);
      WriteLn(F, 'TRIGGER=', TriggerName(W.Events[I].TriggerType));
      WriteLn(F, 'TRIGGERID=', W.Events[I].TriggerID);
      WriteLn(F, 'TRIGGERID2=', W.Events[I].TriggerID2);
      if W.Events[I].OneShot then
        WriteLn(F, 'ONESHOT=1')
      else
        WriteLn(F, 'ONESHOT=0');
      if W.Events[I].Enabled then
        WriteLn(F, 'ENABLED=1')
      else
        WriteLn(F, 'ENABLED=0');
      for J := 1 to W.Events[I].CondCount do
        with W.Events[I].Conditions[J] do
          WriteLn(F, 'COND=', ConditionName(CondType), ',', TargetID, ',',
                  Value, ',', Ord(Negate));
      for J := 1 to W.Events[I].ActionCount do
        with W.Events[I].Actions[J] do
          WriteLn(F, 'ACTION=', ActionName(ActionType), ',', TargetID, ',',
                  Value, ',', Text);
      WriteLn(F);
    end;
  end;

  { Write paragraphs. The body is literal text rather than key=value, so it
    can span lines and keep its blank-line breaks. }
  for I := 1 to W.ParaCount do
  begin
    if W.Paragraphs[I] <> '' then
    begin
      WriteLn(F, '[PARAGRAPH:', I, ']');
      WriteParaBody(F, W.Paragraphs[I]);
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
  { Events deliberately do not enter into this. An event's slot number is its
    identity and slots are never renumbered, so the Fired and EvEnabled
    bitmaps stay meaningful when an author adds or deletes one - and every
    save written before events existed still matches its world. }
end;

function SaveGameState(const FileName: string; var W: TGameWorld): Boolean;
var
  F: File;
  Header: TSaveHeader;
  ObjState: TObjectStateRec;
  MobState: TMobStateRec;
  RoomState: TRoomStateRec;
  Visited: array[0..(MAX_ROOMS div 8) - 1] of Byte;
  Taken: array[0..(MAX_OBJECTS div 8) - 1] of Byte;
  Talked: array[0..(MAX_MOBS div 8) - 1] of Byte;
  Flags: array[0..(MAX_FLAGS div 8) - 1] of Byte;
  Fired: array[0..(MAX_EVENTS div 8) - 1] of Byte;
  EvEnabled: array[0..(MAX_EVENTS div 8) - 1] of Byte;
  I, BytesWritten: Integer;
begin
  Result := False;

  {$I-}
  Assign(F, FileName);
  Rewrite(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;

  { Zeroed whole rather than just Reserved, so the record stays free of
    uninitialised padding if a field is ever added. The two state records
    below are packed numerics with every field assigned - they have none. }
  FillChar(Header, SizeOf(Header), 0);

  Header.Magic := SORS_MAGIC;
  Header.Version := SAVE_VERSION;
  Header.WorldSig := WorldSignature(W);
  Header.CurrentRoom := W.CurrentRoom;
  Header.Score := W.Score;
  Header.Turns := W.Turns;
  Header.InvCount := W.PlayerInvCount;

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

  { Visited rooms, already-scored objects and already-greeted mobs, one bit
    each. These also gate the first-visit/take/talk story paragraphs, so
    saving them is what stops a restored game replaying scenes. }
  FillChar(Visited, SizeOf(Visited), 0);
  for I := 1 to MAX_ROOMS do
    if W.Visited[I] then
      Visited[(I - 1) div 8] := Visited[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  FillChar(Taken, SizeOf(Taken), 0);
  for I := 1 to MAX_OBJECTS do
    if W.Taken[I] then
      Taken[(I - 1) div 8] := Taken[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  FillChar(Talked, SizeOf(Talked), 0);
  for I := 1 to MAX_MOBS do
    if W.Talked[I] then
      Talked[(I - 1) div 8] := Talked[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  {$I-}
  BlockWrite(F, Visited, SizeOf(Visited), BytesWritten);
  BlockWrite(F, Taken, SizeOf(Taken), BytesWritten);
  BlockWrite(F, Talked, SizeOf(Talked), BytesWritten);
  {$I+}
  if IOResult <> 0 then
  begin
    Close(F);
    Exit;
  end;

  { Version 3: event state. Without this a restored game would forget every
    flag it had set and replay every one-shot event it had already fired.
    Counters are written as they stand - they are signed and small, and
    packing them to bits would gain 64 bytes for a lot of fiddling. }
  FillChar(Flags, SizeOf(Flags), 0);
  for I := 1 to MAX_FLAGS do
    if W.Flags[I] then
      Flags[(I - 1) div 8] := Flags[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  FillChar(Fired, SizeOf(Fired), 0);
  for I := 1 to MAX_EVENTS do
    if W.Fired[I] then
      Fired[(I - 1) div 8] := Fired[(I - 1) div 8] or (1 shl ((I - 1) mod 8));

  FillChar(EvEnabled, SizeOf(EvEnabled), 0);
  for I := 1 to MAX_EVENTS do
    if W.EvEnabled[I] then
      EvEnabled[(I - 1) div 8] := EvEnabled[(I - 1) div 8] or
                                  (1 shl ((I - 1) mod 8));

  {$I-}
  BlockWrite(F, Flags, SizeOf(Flags), BytesWritten);
  BlockWrite(F, W.Counters, SizeOf(TCounterArray), BytesWritten);
  BlockWrite(F, Fired, SizeOf(Fired), BytesWritten);
  BlockWrite(F, EvEnabled, SizeOf(EvEnabled), BytesWritten);
  {$I+}
  if IOResult <> 0 then
  begin
    Close(F);
    Exit;
  end;

  { Room exits, which events can lock and unlock }
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then
    begin
      RoomState.ID := W.Rooms[I].ID;
      RoomState.Exits := W.Rooms[I].Exits;
      {$I-}
      BlockWrite(F, RoomState, SizeOf(TRoomStateRec), BytesWritten);
      {$I+}
      if IOResult <> 0 then
      begin
        Close(F);
        Exit;
      end;
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
  Talked: array[0..(MAX_MOBS div 8) - 1] of Byte;
  Flags: array[0..(MAX_FLAGS div 8) - 1] of Byte;
  Fired: array[0..(MAX_EVENTS div 8) - 1] of Byte;
  EvEnabled: array[0..(MAX_EVENTS div 8) - 1] of Byte;
  Counters: TCounterArray;
  RoomState: TRoomStateRec;
  Inv: TInventory;
  I, Idx, BytesRead, BytesRead2: Integer;
  ActiveObjs, ActiveMobs, ActiveRooms: Integer;
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
     (Header.Magic <> SORS_MAGIC) or (Header.Version < 1) or
     (Header.Version > SAVE_VERSION) or
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
  { Version 1 predates the Talked bitmap and is simply that much shorter }
  if Header.Version >= 2 then
    Expected := Expected + SizeOf(Talked);
  { Version 3 appends the event state and the room exits on the same principle }
  ActiveRooms := 0;
  for I := 1 to MAX_ROOMS do
    if W.Rooms[I].Active then Inc(ActiveRooms);
  if Header.Version >= 3 then
    Expected := Expected + SizeOf(Flags) + SizeOf(TCounterArray) +
                SizeOf(Fired) + SizeOf(EvEnabled) +
                ActiveRooms * SizeOf(TRoomStateRec);
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

  FillChar(Talked, SizeOf(Talked), 0);
  if Header.Version >= 2 then
  begin
    {$I-}
    BlockRead(F, Talked, SizeOf(Talked), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(Talked)) then
    begin
      Close(F);
      Exit;
    end;
  end;

  { Version 3 event state. A version 1 or 2 save predates it, so the world
    keeps the cleared state InitWorld gave it - every flag false, every
    counter zero, nothing fired, everything enabled. }
  FillChar(Flags, SizeOf(Flags), 0);
  FillChar(Fired, SizeOf(Fired), 0);
  FillChar(EvEnabled, SizeOf(EvEnabled), $FF);   { Enabled by default }
  FillChar(Counters, SizeOf(Counters), 0);
  if Header.Version >= 3 then
  begin
    {$I-}
    BlockRead(F, Flags, SizeOf(Flags), BytesRead);
    if BytesRead = SizeOf(Flags) then
      BlockRead(F, Counters, SizeOf(TCounterArray), BytesRead2);
    if BytesRead2 = SizeOf(TCounterArray) then
      BlockRead(F, Fired, SizeOf(Fired), BytesRead);
    if BytesRead = SizeOf(Fired) then
      BlockRead(F, EvEnabled, SizeOf(EvEnabled), BytesRead);
    {$I+}
    if (IOResult <> 0) or (BytesRead <> SizeOf(EvEnabled)) or
       (BytesRead2 <> SizeOf(TCounterArray)) then
    begin
      Close(F);
      Exit;
    end;

    for I := 1 to MAX_ROOMS do
      if W.Rooms[I].Active then
      begin
        {$I-}
        BlockRead(F, RoomState, SizeOf(TRoomStateRec), BytesRead);
        {$I+}
        if (IOResult <> 0) or (BytesRead <> SizeOf(TRoomStateRec)) then
        begin
          Close(F);
          Exit;
        end;
        Idx := FindRoomByID(W, RoomState.ID);
        if Idx > 0 then
          W.Rooms[Idx].Exits := RoomState.Exits;
      end;
  end;

  for I := 1 to MAX_ROOMS do
    W.Visited[I] := (Visited[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
  for I := 1 to MAX_OBJECTS do
    W.Taken[I] := (Taken[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
  for I := 1 to MAX_MOBS do
    W.Talked[I] := (Talked[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
  if Header.Version >= 3 then
  begin
    for I := 1 to MAX_FLAGS do
      W.Flags[I] := (Flags[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
    for I := 1 to MAX_EVENTS do
    begin
      W.Fired[I] := (Fired[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
      W.EvEnabled[I] :=
        (EvEnabled[(I - 1) div 8] and (1 shl ((I - 1) mod 8))) <> 0;
    end;
    W.Counters := Counters;
  end
  else
  begin
    { A version 1 or 2 save predates events entirely. Reset rather than leave
      alone: the world in memory may already have been played, so its flags
      and counters hold that run's state, not this save's. Enabled comes from
      what the author wrote, not from a blanket True. }
    for I := 1 to MAX_FLAGS do
      W.Flags[I] := False;
    for I := 1 to MAX_COUNTERS do
      W.Counters[I] := 0;
    SeedEventState(W);
  end;

  W.PlayerInventory := Inv;
  W.PlayerInvCount := Header.InvCount;
  W.CurrentRoom := Header.CurrentRoom;
  W.Score := Header.Score;
  W.Turns := Header.Turns;

  Close(F);
  Result := True;
end;

end.
