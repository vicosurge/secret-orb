{ Secret Orb World Editor }
{ TUI-based room editor for creating game worlds }
program Editor;

{$MODE OBJFPC}

uses
  Crt, SysUtils,
  GameData, DataFile, Display;

const
  VERSION = '0.1.0';

type
  TEditorState = (esMenu, esRoomList, esAddRoom, esEditRoom, esWorldSettings);

var
  World: TGameWorld;
  EditorState: TEditorState;
  CurrentFile: string;
  Modified: Boolean;
  SelectedRoom: Integer;

procedure DrawHeader;
begin
  SetColor(Black, Cyan);
  WriteAt(1, 1, '                    Secret Orb World Editor v' + VERSION + '                    ');
  ResetColor;

  SetColor(DarkGray, Black);
  if CurrentFile <> '' then
    WriteAt(1, 2, 'File: ' + CurrentFile)
  else
    WriteAt(1, 2, 'File: (unsaved)');

  if Modified then
  begin
    SetColor(Yellow, Black);
    WriteAt(70, 2, '[Modified]');
  end;
  ResetColor;
end;

procedure DrawMenu;
var
  Y: Integer;
begin
  ClearScreen;
  DrawHeader;

  Y := 6;
  SetColor(Yellow, Black);
  WriteCenter(Y, '=== MAIN MENU ===');
  ResetColor;

  Inc(Y, 3);
  WriteAt(30, Y, '1. List Rooms');
  Inc(Y, 2);
  WriteAt(30, Y, '2. Add Room');
  Inc(Y, 2);
  WriteAt(30, Y, '3. World Settings');
  Inc(Y, 2);
  WriteAt(30, Y, '4. Load World');
  Inc(Y, 2);
  WriteAt(30, Y, '5. Save World');
  Inc(Y, 2);
  WriteAt(30, Y, '6. New World');
  Inc(Y, 2);
  WriteAt(30, Y, 'Q. Quit');

  SetColor(Cyan, Black);
  WriteAt(1, 24, 'Choice: ');
  ResetColor;
end;

procedure DrawRoomList;
var
  I, Y, Count: Integer;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== ROOM LIST ===');
  ResetColor;

  Y := 6;
  Count := 0;
  for I := 1 to MAX_ROOMS do
  begin
    if World.Rooms[I].Active and (Y < 22) then
    begin
      Inc(Count);
      if I = SelectedRoom then
        SetColor(Black, White)
      else
        SetColor(LightGray, Black);

      WriteAt(3, Y, Format('%3d: %-40s [N:%d S:%d E:%d W:%d]',
        [World.Rooms[I].ID,
         World.Rooms[I].Name,
         World.Rooms[I].Exits[dirNorth],
         World.Rooms[I].Exits[dirSouth],
         World.Rooms[I].Exits[dirEast],
         World.Rooms[I].Exits[dirWest]]));
      Inc(Y);
      ResetColor;
    end;
  end;

  if Count = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteCenter(12, '(No rooms defined)');
    ResetColor;
  end;

  SetColor(Cyan, Black);
  WriteAt(1, 23, 'Up/Down: Select  E: Edit  D: Delete  A: Add  Esc: Back');
  ResetColor;
end;

procedure EditRoomForm(RoomIdx: Integer; IsNew: Boolean);
var
  R: TRoom;
  S: string;
  Field: Integer;
begin
  if IsNew then
  begin
    InitRoom(R);
    R.ID := World.RoomCount + 1;
    R.Active := True;
  end
  else
    R := World.Rooms[RoomIdx];

  Field := 0;

  repeat
    ClearScreen;
    DrawHeader;

    if IsNew then
      SetColor(Yellow, Black)
    else
      SetColor(LightGreen, Black);

    if IsNew then
      WriteCenter(4, '=== ADD NEW ROOM ===')
    else
      WriteCenter(4, '=== EDIT ROOM ===');
    ResetColor;

    { Display fields }
    WriteAt(5, 7, 'ID:          ');
    WriteAt(20, 7, IntToStr(R.ID));

    if Field = 0 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 9, 'Name:        ');
    WriteAt(20, 9, R.Name + '                                        ');
    ResetColor;

    if Field = 1 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 11, 'Description: ');
    WriteAt(20, 11, R.Desc);
    ResetColor;

    if Field = 2 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 13, 'North Exit:  ');
    WriteAt(20, 13, IntToStr(R.Exits[dirNorth]) + '    ');
    ResetColor;

    if Field = 3 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 14, 'South Exit:  ');
    WriteAt(20, 14, IntToStr(R.Exits[dirSouth]) + '    ');
    ResetColor;

    if Field = 4 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 15, 'East Exit:   ');
    WriteAt(20, 15, IntToStr(R.Exits[dirEast]) + '    ');
    ResetColor;

    if Field = 5 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 16, 'West Exit:   ');
    WriteAt(20, 16, IntToStr(R.Exits[dirWest]) + '    ');
    ResetColor;

    SetColor(Cyan, Black);
    WriteAt(1, 20, 'Tab: Next Field  Enter: Edit Field  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 6;
      #13: { Enter - edit current field }
        begin
          case Field of
            0: begin
                 S := ReadLine(20, 9, MAX_NAME_LEN);
                 if S <> '' then R.Name := S;
               end;
            1: begin
                 S := ReadLine(20, 11, MAX_DESC_LEN);
                 if S <> '' then R.Desc := S;
               end;
            2: begin
                 S := ReadLine(20, 13, 5);
                 R.Exits[dirNorth] := StrToIntDef(S, R.Exits[dirNorth]);
               end;
            3: begin
                 S := ReadLine(20, 14, 5);
                 R.Exits[dirSouth] := StrToIntDef(S, R.Exits[dirSouth]);
               end;
            4: begin
                 S := ReadLine(20, 15, 5);
                 R.Exits[dirEast] := StrToIntDef(S, R.Exits[dirEast]);
               end;
            5: begin
                 S := ReadLine(20, 16, 5);
                 R.Exits[dirWest] := StrToIntDef(S, R.Exits[dirWest]);
               end;
          end;
        end;
      #0: { Extended key }
        case ReadKey of
          #60: { F2 - Save }
            begin
              if IsNew then
              begin
                Inc(World.RoomCount);
                RoomIdx := World.RoomCount;
              end;
              World.Rooms[RoomIdx] := R;
              Modified := True;
              Exit;
            end;
          #72: { Up }
            if Field > 0 then Dec(Field);
          #80: { Down }
            if Field < 5 then Inc(Field);
        end;
      #27: { Escape }
        Exit;
    end;
  until False;
end;

procedure DeleteRoom(RoomIdx: Integer);
var
  Ch: Char;
begin
  SetColor(LightRed, Black);
  WriteAt(1, 24, 'Delete "' + World.Rooms[RoomIdx].Name + '"? (Y/N) ');
  ResetColor;

  Ch := ReadKey;
  if UpCase(Ch) = 'Y' then
  begin
    World.Rooms[RoomIdx].Active := False;
    Modified := True;
  end;
end;

procedure HandleRoomList;
var
  Ch: Char;
  I: Integer;
begin
  SelectedRoom := 0;
  { Find first active room }
  for I := 1 to MAX_ROOMS do
    if World.Rooms[I].Active then
    begin
      SelectedRoom := I;
      Break;
    end;

  repeat
    DrawRoomList;
    Ch := ReadKey;

    case Ch of
      #0: { Extended key }
        case ReadKey of
          #72: { Up }
            begin
              for I := SelectedRoom - 1 downto 1 do
                if World.Rooms[I].Active then
                begin
                  SelectedRoom := I;
                  Break;
                end;
            end;
          #80: { Down }
            begin
              for I := SelectedRoom + 1 to MAX_ROOMS do
                if World.Rooms[I].Active then
                begin
                  SelectedRoom := I;
                  Break;
                end;
            end;
        end;
      'e', 'E':
        if SelectedRoom > 0 then
          EditRoomForm(SelectedRoom, False);
      'd', 'D':
        if SelectedRoom > 0 then
          DeleteRoom(SelectedRoom);
      'a', 'A':
        EditRoomForm(0, True);
      #27: { Escape }
        Exit;
    end;
  until False;
end;

procedure WorldSettings;
var
  S: string;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== WORLD SETTINGS ===');
  ResetColor;

  WriteAt(5, 8, 'Title:       ');
  WriteAt(20, 8, World.Title);

  WriteAt(5, 10, 'Start Room:  ');
  WriteAt(20, 10, IntToStr(World.CurrentRoom));

  WriteAt(5, 12, 'Room Count:  ');
  WriteAt(20, 12, IntToStr(World.RoomCount));

  SetColor(Cyan, Black);
  WriteAt(1, 16, 'Press T to change title, S to change start room, Esc to return');
  ResetColor;

  case UpCase(ReadKey) of
    'T':
      begin
        S := ReadLine(20, 8, MAX_NAME_LEN);
        if S <> '' then
        begin
          World.Title := S;
          Modified := True;
        end;
      end;
    'S':
      begin
        S := ReadLine(20, 10, 5);
        if S <> '' then
        begin
          World.CurrentRoom := StrToIntDef(S, World.CurrentRoom);
          Modified := True;
        end;
      end;
  end;
end;

procedure LoadWorldFile;
var
  S: string;
begin
  ClearScreen;
  DrawHeader;

  WriteAt(5, 10, 'Enter filename: ');
  S := ReadLine(22, 10, 60);

  if S <> '' then
  begin
    if LoadWorld(S, World) then
    begin
      CurrentFile := S;
      Modified := False;
    end
    else
    begin
      SetColor(LightRed, Black);
      WriteAt(5, 12, 'Error loading file!');
      ResetColor;
      Delay(1500);
    end;
  end;
end;

procedure SaveWorldFile;
var
  S: string;
begin
  ClearScreen;
  DrawHeader;

  WriteAt(5, 10, 'Save as [' + CurrentFile + ']: ');
  S := ReadLine(30, 10, 60);

  if S = '' then S := CurrentFile;
  if S = '' then S := 'world.dat';

  if SaveWorld(S, World) then
  begin
    CurrentFile := S;
    Modified := False;
    SetColor(LightGreen, Black);
    WriteAt(5, 12, 'World saved successfully!');
  end
  else
  begin
    SetColor(LightRed, Black);
    WriteAt(5, 12, 'Error saving file!');
  end;
  ResetColor;
  Delay(1500);
end;

procedure NewWorld;
var
  Ch: Char;
begin
  if Modified then
  begin
    SetColor(Yellow, Black);
    WriteAt(1, 24, 'Discard unsaved changes? (Y/N) ');
    ResetColor;
    Ch := ReadKey;
    if UpCase(Ch) <> 'Y' then Exit;
  end;

  InitWorld(World);
  CurrentFile := '';
  Modified := False;
end;

procedure MainLoop;
var
  Ch: Char;
  Running: Boolean;
begin
  Running := True;

  while Running do
  begin
    DrawMenu;
    Ch := ReadKey;

    case UpCase(Ch) of
      '1': HandleRoomList;
      '2': EditRoomForm(0, True);
      '3': WorldSettings;
      '4': LoadWorldFile;
      '5': SaveWorldFile;
      '6': NewWorld;
      'Q':
        begin
          if Modified then
          begin
            SetColor(Yellow, Black);
            WriteAt(1, 24, 'Save before quitting? (Y/N/Esc) ');
            ResetColor;
            Ch := ReadKey;
            case UpCase(Ch) of
              'Y': SaveWorldFile;
              #27: Continue;
            end;
          end;
          Running := False;
        end;
    end;
  end;
end;

begin
  InitWorld(World);
  CurrentFile := '';
  Modified := False;
  EditorState := esMenu;

  ClrScr;
  CursorOff;

  { Check for command line file }
  if ParamCount > 0 then
  begin
    if LoadWorld(ParamStr(1), World) then
      CurrentFile := ParamStr(1);
  end;

  MainLoop;

  ClrScr;
  CursorOn;
end.
