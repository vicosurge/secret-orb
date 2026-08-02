{ display.pas - Text display abstraction for Secret Orb }
unit Display;

{$MODE OBJFPC}

interface

uses
  Crt;

const
  SCREEN_WIDTH = 80;
  SCREEN_HEIGHT = 25;
  MAX_WRAP_LINES = 64;
  PRESS_ANY_KEY = 'Press any key to continue...';

type
  TWrapLines = array[1..MAX_WRAP_LINES] of string;

procedure InitDisplay;
procedure ClearScreen;
procedure WriteAt(X, Y: Integer; const S: string);
procedure WriteCenter(Y: Integer; const S: string);
procedure WriteWrapped(X, Y, Width: Integer; const S: string);
function WrapText(const S: AnsiString; Width: Integer;
                  var Lines: TWrapLines; MaxLines: Integer): Integer;
procedure ShowTextPage(const Heading: string; const Body: AnsiString);
procedure SetColor(FG, BG: Byte);
procedure ResetColor;
procedure WaitKey;
function ReadLine(X, Y, MaxLen: Integer): string;
procedure DrawBox(X1, Y1, X2, Y2: Integer);
procedure DrawHLine(X1, X2, Y: Integer);

implementation

{ Shared scratch buffer. A TWrapLines is 16KB, which is more than we want on
  the DOS stack, and nothing here wraps text reentrantly. }
var
  WrapBuf: TWrapLines;

procedure InitDisplay;
begin
  ClrScr;
  CursorOff;
end;

procedure ClearScreen;
begin
  ClrScr;
end;

procedure WriteAt(X, Y: Integer; const S: string);
begin
  GotoXY(X, Y);
  Write(S);
end;

procedure WriteCenter(Y: Integer; const S: string);
var
  X: Integer;
begin
  X := (SCREEN_WIDTH - Length(S)) div 2 + 1;
  if X < 1 then X := 1;
  GotoXY(X, Y);
  Write(S);
end;

{ Word-wraps S to Width columns, returning the number of lines produced.
  #13, #10 and #13#10 are hard breaks, so a blank line survives as a blank
  line. Wrapping stops once MaxLines is reached rather than overrunning. }
function WrapText(const S: AnsiString; Width: Integer;
                  var Lines: TWrapLines; MaxLines: Integer): Integer;
var
  I, LineStart, LastSpace, Count: Integer;
  Ch: Char;

  procedure Emit(const T: string);
  begin
    if Count < MaxLines then
    begin
      Inc(Count);
      Lines[Count] := T;
    end;
  end;

begin
  Count := 0;
  if Width < 1 then Width := 1;
  if MaxLines > MAX_WRAP_LINES then MaxLines := MAX_WRAP_LINES;
  LineStart := 1;
  LastSpace := 0;

  I := 1;
  while (I <= Length(S)) and (Count < MaxLines) do
  begin
    Ch := S[I];
    if (Ch = #13) or (Ch = #10) then
    begin
      Emit(Copy(S, LineStart, I - LineStart));
      { CRLF is one break, not two }
      if (Ch = #13) and (I < Length(S)) and (S[I + 1] = #10) then Inc(I);
      LineStart := I + 1;
      LastSpace := 0;
    end
    else
    begin
      if Ch = ' ' then LastSpace := I;
      if (I - LineStart + 1) >= Width then
      begin
        if LastSpace > LineStart then
        begin
          { Break at the last space; the tail is rescanned from LineStart }
          Emit(Copy(S, LineStart, LastSpace - LineStart));
          LineStart := LastSpace + 1;
        end
        else
        begin
          { A single word longer than Width has to be split mid-word }
          Emit(Copy(S, LineStart, I - LineStart + 1));
          LineStart := I + 1;
        end;
        LastSpace := 0;
        while (LineStart <= Length(S)) and (S[LineStart] = ' ') do Inc(LineStart);
        I := LineStart - 1;
      end;
    end;
    Inc(I);
  end;

  if (LineStart <= Length(S)) and (Count < MaxLines) then
    Emit(Copy(S, LineStart, Length(S) - LineStart + 1));
  Result := Count;
end;

procedure WriteWrapped(X, Y, Width: Integer; const S: string);
var
  I, Count: Integer;
begin
  Count := WrapText(S, Width, WrapBuf, MAX_WRAP_LINES);
  for I := 1 to Count do
    WriteAt(X, Y + I - 1, WrapBuf[I]);
end;

{ Shows Body a screenful at a time, pausing between pages. Used for anything
  longer than a status line - story paragraphs, help, endings. }
procedure ShowTextPage(const Heading: string; const Body: AnsiString);
var
  Total, Idx, Row, FirstRow, LastRow: Integer;
begin
  Total := WrapText(Body, SCREEN_WIDTH - 6, WrapBuf, MAX_WRAP_LINES);
  if Total = 0 then Exit;

  if Heading <> '' then FirstRow := 4 else FirstRow := 2;
  LastRow := SCREEN_HEIGHT - 2;

  Idx := 1;
  while Idx <= Total do
  begin
    ClearScreen;
    if Heading <> '' then
    begin
      SetColor(Yellow, Black);
      WriteCenter(1, Heading);
      ResetColor;
      DrawHLine(1, SCREEN_WIDTH, 2);
    end;

    Row := FirstRow;
    while (Row <= LastRow) and (Idx <= Total) do
    begin
      WriteAt(3, Row, WrapBuf[Idx]);
      Inc(Row);
      Inc(Idx);
    end;

    SetColor(Cyan, Black);
    if Idx <= Total then
      WriteCenter(SCREEN_HEIGHT, '-- More --')
    else
      WriteCenter(SCREEN_HEIGHT, PRESS_ANY_KEY);
    ResetColor;
    WaitKey;
  end;
end;

procedure SetColor(FG, BG: Byte);
begin
  TextColor(FG);
  TextBackground(BG);
end;

procedure ResetColor;
begin
  TextColor(LightGray);
  TextBackground(Black);
end;

procedure WaitKey;
begin
  ReadKey;
end;

function ReadLine(X, Y, MaxLen: Integer): string;
var
  S: string;
  Ch: Char;
begin
  S := '';
  CursorOn;
  GotoXY(X, Y);

  repeat
    Ch := ReadKey;
    case Ch of
      #8: { Backspace }
        if Length(S) > 0 then
        begin
          Delete(S, Length(S), 1);
          GotoXY(X + Length(S), Y);
          Write(' ');
          GotoXY(X + Length(S), Y);
        end;
      #13: { Enter }
        Break;
      #27: { Escape }
        begin
          S := '';
          Break;
        end;
      #32..#126: { Printable characters }
        if Length(S) < MaxLen then
        begin
          S := S + Ch;
          Write(Ch);
        end;
    end;
  until False;

  CursorOff;
  Result := S;
end;

procedure DrawBox(X1, Y1, X2, Y2: Integer);
var
  I: Integer;
begin
  { Corners }
  WriteAt(X1, Y1, '+');
  WriteAt(X2, Y1, '+');
  WriteAt(X1, Y2, '+');
  WriteAt(X2, Y2, '+');

  { Horizontal lines }
  for I := X1 + 1 to X2 - 1 do
  begin
    WriteAt(I, Y1, '-');
    WriteAt(I, Y2, '-');
  end;

  { Vertical lines }
  for I := Y1 + 1 to Y2 - 1 do
  begin
    WriteAt(X1, I, '|');
    WriteAt(X2, I, '|');
  end;
end;

procedure DrawHLine(X1, X2, Y: Integer);
var
  I: Integer;
begin
  for I := X1 to X2 do
    WriteAt(I, Y, '-');
end;

end.
