{ display.pas - Text display abstraction for Secret Orb }
unit Display;

{$MODE OBJFPC}

interface

uses
  Crt;

const
  SCREEN_WIDTH = 80;
  SCREEN_HEIGHT = 25;

procedure InitDisplay;
procedure ClearScreen;
procedure WriteAt(X, Y: Integer; const S: string);
procedure WriteCenter(Y: Integer; const S: string);
procedure WriteWrapped(X, Y, Width: Integer; const S: string);
procedure SetColor(FG, BG: Byte);
procedure ResetColor;
procedure WaitKey;
function ReadLine(X, Y, MaxLen: Integer): string;
procedure DrawBox(X1, Y1, X2, Y2: Integer);
procedure DrawHLine(X1, X2, Y: Integer);

implementation

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

procedure WriteWrapped(X, Y, Width: Integer; const S: string);
var
  I, LineStart, LastSpace, Col: Integer;
  CurrentY: Integer;
begin
  CurrentY := Y;
  LineStart := 1;
  LastSpace := 0;
  Col := 0;

  for I := 1 to Length(S) do
  begin
    if S[I] = ' ' then
      LastSpace := I;
    Inc(Col);

    if Col >= Width then
    begin
      GotoXY(X, CurrentY);
      if LastSpace > LineStart then
      begin
        Write(Copy(S, LineStart, LastSpace - LineStart));
        LineStart := LastSpace + 1;
      end
      else
      begin
        Write(Copy(S, LineStart, I - LineStart));
        LineStart := I;
      end;
      Inc(CurrentY);
      Col := I - LineStart + 1;
      LastSpace := 0;
    end;
  end;

  { Write remaining text }
  if LineStart <= Length(S) then
  begin
    GotoXY(X, CurrentY);
    Write(Copy(S, LineStart, Length(S) - LineStart + 1));
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
