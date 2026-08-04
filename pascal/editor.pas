{ Secret Orb World Editor }
{ TUI-based room editor for creating game worlds }
program Editor;

{$MODE OBJFPC}

uses
  Crt, SysUtils,
  GameData, DataFile, Display, WorldVal;

const
  VERSION = '0.1.0';
  PARA_COLS = 74;         { Editing grid width, fits the 80-column screen }

type
  TEditorState = (esMenu, esRoomList, esAddRoom, esEditRoom, esWorldSettings,
                  esObjectList, esAddObject, esEditObject,
                  esMobList, esAddMob, esEditMob,
                  esParagraphList, esEditParagraph,
                  esEventList, esViewEvent);

  TParaLines = array[1..MAX_PARA_LINES] of string;

var
  World: TGameWorld;
  { Held here rather than on the stack - a TIssueList is about 15KB, which is
    more than a DOS stack wants to carry. }
  Issues: TIssueList;
  EditorState: TEditorState;
  CurrentFile: string;
  Modified: Boolean;
  SelectedRoom: Integer;
  SelectedObject: Integer;
  SelectedMob: Integer;
  SelectedPara: Integer;
  SelectedEvent: Integer;

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

  Y := 5;
  SetColor(Yellow, Black);
  WriteCenter(Y, '=== MAIN MENU ===');
  ResetColor;

  Inc(Y, 2);
  WriteAt(30, Y, '1. List Rooms');
  Inc(Y, 1);
  WriteAt(30, Y, '2. Add Room');
  Inc(Y, 2);
  WriteAt(30, Y, '3. List Objects');
  Inc(Y, 1);
  WriteAt(30, Y, '4. Add Object');
  Inc(Y, 2);
  WriteAt(30, Y, '5. List Mobs');
  Inc(Y, 1);
  WriteAt(30, Y, '6. Add Mob');
  Inc(Y, 2);
  WriteAt(30, Y, 'P. Story Paragraphs');
  Inc(Y, 1);
  WriteAt(30, Y, 'E. Events');
  Inc(Y, 2);
  WriteAt(30, Y, 'V. Validate World');
  Inc(Y, 2);
  WriteAt(30, Y, '7. World Settings');
  Inc(Y, 1);
  WriteAt(30, Y, '8. Load World');
  Inc(Y, 1);
  WriteAt(30, Y, '9. Save World');
  Inc(Y, 1);
  WriteAt(30, Y, '0. New World');
  Inc(Y, 1);
  WriteAt(30, Y, 'Q. Quit');

  SetColor(Cyan, Black);
  WriteAt(1, 24, 'Choice: ');
  ResetColor;
end;

{ Runs the same checks as editor-tv and the browser editor, and pages the
  results. Errors are things the engine will get wrong; warnings are things an
  author usually meant to do differently. }
procedure ValidateForm;
var
  Count, Errors, First, I, Y: Integer;
  Ch: Char;
begin
  Count := ValidateWorld(World, Issues);

  Errors := 0;
  for I := 1 to Count do
    if Issues[I].Level = ilError then Inc(Errors);

  First := 1;
  repeat
    ClearScreen;
    DrawHeader;

    SetColor(Yellow, Black);
    WriteCenter(4, '=== WORLD CHECK ===');
    ResetColor;

    if Count = 0 then
    begin
      SetColor(LightGreen, Black);
      WriteCenter(12, 'No problems found.');
      ResetColor;
    end
    else
    begin
      Y := 6;
      I := First;
      while (I <= Count) and (Y < 22) do
      begin
        if Issues[I].Level = ilError then
          SetColor(LightRed, Black)
        else
          SetColor(Yellow, Black);
        WriteAt(1, Y, IssueLevelName(Issues[I].Level));
        ResetColor;
        WriteAt(7, Y, Copy('[' + Issues[I].Where + '] ' + Issues[I].Text,
                           1, 72));
        Inc(Y);
        Inc(I);
      end;
    end;

    SetColor(Cyan, Black);
    WriteAt(1, 23, IntToStr(Errors) + ' error(s), ' +
                   IntToStr(Count - Errors) + ' warning(s).' +
                   '  PgUp/PgDn: Scroll  Esc: Back');
    ResetColor;

    Ch := ReadKey;
    if Ch = #0 then
    begin
      Ch := ReadKey;
      case Ch of
        #73: { PgUp }
          begin
            Dec(First, 16);
            if First < 1 then First := 1;
          end;
        #81: { PgDn }
          if First + 16 <= Count then Inc(First, 16);
      end;
      Ch := #1;   { Not Esc - keep the screen up }
    end;
  until Ch = #27;
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

      WriteAt(3, Y, Format('%3d: %-32s [N:%d S:%d E:%d W:%d U:%d D:%d]',
        [World.Rooms[I].ID,
         World.Rooms[I].Name,
         World.Rooms[I].Exits[dirNorth],
         World.Rooms[I].Exits[dirSouth],
         World.Rooms[I].Exits[dirEast],
         World.Rooms[I].Exits[dirWest],
         World.Rooms[I].Exits[dirUp],
         World.Rooms[I].Exits[dirDown]]));
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

{ Offers to pair up the exits of a room just saved - one prompt for the whole
  form rather than one per field. PairExits does the work so that this editor
  and editor-tv behave identically. }
procedure OfferReverseExits(RoomIdx: Integer);
var
  Count: Integer;
  Ch: Char;
begin
  Count := PairExits(World, RoomIdx, False);
  if Count = 0 then Exit;

  SetColor(Yellow, Black);
  WriteAt(1, 24, 'Create ' + IntToStr(Count) +
                 ' matching return exit(s)? (Y/N) ');
  ResetColor;
  Ch := ReadKey;
  if UpCase(Ch) <> 'Y' then Exit;

  PairExits(World, RoomIdx, True);
  Modified := True;
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

    if Field = 6 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 17, 'Up Exit:     ');
    WriteAt(20, 17, IntToStr(R.Exits[dirUp]) + '    ');
    ResetColor;

    if Field = 7 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 18, 'Down Exit:   ');
    WriteAt(20, 18, IntToStr(R.Exits[dirDown]) + '    ');
    ResetColor;

    if Field = 8 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 19, 'Points:      ');
    WriteAt(20, 19, IntToStr(R.Points) + '    ');
    ResetColor;

    if Field = 9 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 20, 'First Visit: ');
    WriteAt(20, 20, IntToStr(R.FirstVisitPara) + '    ');
    ResetColor;

    SetColor(Cyan, Black);
    WriteAt(1, 21, 'Points and the First Visit paragraph both fire once, on arrival.');
    WriteAt(1, 22, 'Tab: Next Field  Enter: Edit Field  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 10;
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
            6: begin
                 S := ReadLine(20, 17, 5);
                 R.Exits[dirUp] := StrToIntDef(S, R.Exits[dirUp]);
               end;
            7: begin
                 S := ReadLine(20, 18, 5);
                 R.Exits[dirDown] := StrToIntDef(S, R.Exits[dirDown]);
               end;
            8: begin
                 S := ReadLine(20, 19, 5);
                 R.Points := StrToIntDef(S, R.Points);
               end;
            9: begin
                 S := ReadLine(20, 20, 5);
                 R.FirstVisitPara := StrToIntDef(S, R.FirstVisitPara);
               end;
          end;
        end;
      #0: { Extended key }
        case ReadKey of
          #60: { F2 - Save }
            begin
              if IsNew then
              begin
                { Bound before incrementing, or RoomCount runs past the array }
                if World.RoomCount >= MAX_ROOMS then
                begin
                  SetColor(LightRed, Black);
                  WriteAt(1, 24, 'Maximum of ' + IntToStr(MAX_ROOMS) +
                                 ' rooms reached. Press any key...');
                  ResetColor;
                  ReadKey;
                  Exit;
                end;
                Inc(World.RoomCount);
                RoomIdx := World.RoomCount;
              end;
              World.Rooms[RoomIdx] := R;
              Modified := True;
              OfferReverseExits(RoomIdx);
              Exit;
            end;
          #72: { Up }
            if Field > 0 then Dec(Field);
          #80: { Down }
            if Field < 9 then Inc(Field);
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

  WriteAt(5, 6, 'Title:       ');
  WriteAt(20, 6, World.Title);

  WriteAt(5, 8, 'Start Room:  ');
  WriteAt(20, 8, IntToStr(World.CurrentRoom));

  WriteAt(5, 9, 'Win Room:    ');
  WriteAt(20, 9, IntToStr(World.WinRoomID));

  WriteAt(5, 10, 'Win Object:  ');
  WriteAt(20, 10, IntToStr(World.WinObjectID));

  WriteAt(5, 12, 'Intro Para:  ');
  WriteAt(20, 12, IntToStr(World.IntroPara));

  WriteAt(5, 13, 'Win Para:    ');
  WriteAt(20, 13, IntToStr(World.WinPara));

  WriteAt(5, 14, 'Lose Para:   ');
  WriteAt(20, 14, IntToStr(World.LosePara));

  WriteAt(5, 15, 'Booklet Mode:');
  if (World.WorldFlags and WF_BOOKLET) <> 0 then
    WriteAt(20, 15, 'ON  (cite numbers, do not print text) ')
  else
    WriteAt(20, 15, 'OFF (print paragraph text in game)    ');

  WriteAt(5, 17, 'Room Count:  ');
  WriteAt(20, 17, IntToStr(World.RoomCount));

  WriteAt(5, 18, 'Max Score:   ');
  WriteAt(20, 18, IntToStr(ComputeMaxScore(World)));

  SetColor(Cyan, Black);
  WriteAt(1, 20, 'The game is won by reaching Win Room while carrying Win Object.');
  WriteAt(1, 21, 'Use 0 for any of the above to disable it. Lose Para is shown when');
  WriteAt(1, 22, 'the player quits without winning.');
  WriteAt(1, 23, 'T=title S=start W=win room O=win obj  I/P/L=paras B=booklet  Esc');
  ResetColor;

  case UpCase(ReadKey) of
    'T':
      begin
        S := ReadLine(20, 6, MAX_NAME_LEN);
        if S <> '' then
        begin
          World.Title := S;
          Modified := True;
        end;
      end;
    'S':
      begin
        S := ReadLine(20, 8, 5);
        if S <> '' then
        begin
          World.CurrentRoom := StrToIntDef(S, World.CurrentRoom);
          Modified := True;
        end;
      end;
    'W':
      begin
        S := ReadLine(20, 9, 5);
        if S <> '' then
        begin
          World.WinRoomID := StrToIntDef(S, World.WinRoomID);
          Modified := True;
        end;
      end;
    'O':
      begin
        S := ReadLine(20, 10, 5);
        if S <> '' then
        begin
          World.WinObjectID := StrToIntDef(S, World.WinObjectID);
          Modified := True;
        end;
      end;
    'I':
      begin
        S := ReadLine(20, 12, 5);
        if S <> '' then
        begin
          World.IntroPara := StrToIntDef(S, World.IntroPara);
          Modified := True;
        end;
      end;
    'P':
      begin
        S := ReadLine(20, 13, 5);
        if S <> '' then
        begin
          World.WinPara := StrToIntDef(S, World.WinPara);
          Modified := True;
        end;
      end;
    'L':
      begin
        S := ReadLine(20, 14, 5);
        if S <> '' then
        begin
          World.LosePara := StrToIntDef(S, World.LosePara);
          Modified := True;
        end;
      end;
    'B':
      begin
        World.WorldFlags := World.WorldFlags xor WF_BOOKLET;
        Modified := True;
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

{ Object editing procedures }

procedure DrawObjectList;
var
  I, Y, Count: Integer;
  FlagStr: string;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== OBJECT LIST ===');
  ResetColor;

  Y := 6;
  Count := 0;
  for I := 1 to MAX_OBJECTS do
  begin
    if World.Objects[I].Active and (Y < 21) then
    begin
      Inc(Count);
      if I = SelectedObject then
        SetColor(Black, White)
      else
        SetColor(LightGray, Black);

      FlagStr := '';
      if ofPickup in World.Objects[I].Flags then FlagStr := FlagStr + 'P';
      if ofUse in World.Objects[I].Flags then FlagStr := FlagStr + 'U';
      if ofOpen in World.Objects[I].Flags then FlagStr := FlagStr + 'O';
      if ofRead in World.Objects[I].Flags then FlagStr := FlagStr + 'R';

      WriteAt(3, Y, Format('%3d: %-25s Room:%3d [%s]',
        [World.Objects[I].ID,
         World.Objects[I].Name,
         World.Objects[I].RoomID,
         FlagStr]));
      Inc(Y);
      ResetColor;
    end;
  end;

  if Count = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteCenter(12, '(No objects defined)');
    ResetColor;
  end;

  SetColor(Cyan, Black);
  WriteAt(1, 23, 'Up/Down: Select  E: Edit  D: Delete  A: Add  Esc: Back');
  ResetColor;
end;

procedure EditObjectForm(ObjIdx: Integer; IsNew: Boolean);
var
  O: TGameObject;
  S: string;
  Field: Integer;
begin
  if IsNew then
  begin
    InitObject(O);
    O.ID := World.ObjectCount + 1;
    O.Active := True;
  end
  else
    O := World.Objects[ObjIdx];

  Field := 0;

  repeat
    ClearScreen;
    DrawHeader;

    if IsNew then
      SetColor(Yellow, Black)
    else
      SetColor(LightGreen, Black);

    if IsNew then
      WriteCenter(4, '=== ADD NEW OBJECT ===')
    else
      WriteCenter(4, '=== EDIT OBJECT ===');
    ResetColor;

    { Display fields }
    WriteAt(5, 6, 'ID:          ');
    WriteAt(20, 6, IntToStr(O.ID));

    if Field = 0 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 7, 'Name:        ');
    WriteAt(20, 7, O.Name + '                              ');
    ResetColor;

    if Field = 1 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 8, 'Description: ');
    WriteAt(20, 8, O.Desc);
    ResetColor;

    if Field = 2 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 9, 'Room ID:     ');
    WriteAt(20, 9, IntToStr(O.RoomID) + '    ');
    ResetColor;

    if Field = 3 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 10, 'Pickup:      ');
    if ofPickup in O.Flags then WriteAt(20, 10, '[X]') else WriteAt(20, 10, '[ ]');
    ResetColor;

    if Field = 4 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 11, 'Use:         ');
    if ofUse in O.Flags then WriteAt(20, 11, '[X]') else WriteAt(20, 11, '[ ]');
    ResetColor;

    if Field = 5 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 12, 'Open:        ');
    if ofOpen in O.Flags then WriteAt(20, 12, '[X]') else WriteAt(20, 12, '[ ]');
    ResetColor;

    if Field = 6 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 13, 'Read:        ');
    if ofRead in O.Flags then WriteAt(20, 13, '[X]') else WriteAt(20, 13, '[ ]');
    ResetColor;

    if Field = 7 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 14, 'Use Text:    ');
    WriteAt(20, 14, O.UseText);
    ResetColor;

    if Field = 8 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 15, 'Points:      ');
    WriteAt(20, 15, IntToStr(O.Points) + '    ');
    ResetColor;

    if Field = 9 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 16, 'First Take:  ');
    WriteAt(20, 16, IntToStr(O.FirstTakePara) + '    ');
    ResetColor;

    SetColor(Cyan, Black);
    WriteAt(1, 17, 'Points and the First Take paragraph both fire once, on first take.');
    WriteAt(1, 18, 'Tab/Arrows: Navigate  Enter: Edit/Toggle  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 10;
      #13: { Enter }
        begin
          case Field of
            0: begin
                 S := ReadLine(20, 7, MAX_OBJ_NAME);
                 if S <> '' then O.Name := S;
               end;
            1: begin
                 S := ReadLine(20, 8, MAX_OBJ_DESC);
                 if S <> '' then O.Desc := S;
               end;
            2: begin
                 S := ReadLine(20, 9, 5);
                 O.RoomID := StrToIntDef(S, O.RoomID);
               end;
            3: { Toggle Pickup }
               if ofPickup in O.Flags then
                 Exclude(O.Flags, ofPickup)
               else
                 Include(O.Flags, ofPickup);
            4: { Toggle Use }
               if ofUse in O.Flags then
                 Exclude(O.Flags, ofUse)
               else
                 Include(O.Flags, ofUse);
            5: { Toggle Open }
               if ofOpen in O.Flags then
                 Exclude(O.Flags, ofOpen)
               else
                 Include(O.Flags, ofOpen);
            6: { Toggle Read }
               if ofRead in O.Flags then
                 Exclude(O.Flags, ofRead)
               else
                 Include(O.Flags, ofRead);
            7: begin
                 S := ReadLine(20, 14, MAX_OBJ_DESC);
                 if S <> '' then O.UseText := S;
               end;
            8: begin
                 S := ReadLine(20, 15, 5);
                 O.Points := StrToIntDef(S, O.Points);
               end;
            9: begin
                 S := ReadLine(20, 16, 5);
                 O.FirstTakePara := StrToIntDef(S, O.FirstTakePara);
               end;
          end;
        end;
      #0: { Extended key }
        case ReadKey of
          #60: { F2 - Save }
            begin
              if IsNew then
              begin
                Inc(World.ObjectCount);
                ObjIdx := World.ObjectCount;
              end;
              World.Objects[ObjIdx] := O;
              Modified := True;
              Exit;
            end;
          #72: { Up }
            if Field > 0 then Dec(Field);
          #80: { Down }
            if Field < 9 then Inc(Field);
        end;
      #27: { Escape }
        Exit;
    end;
  until False;
end;

procedure DeleteObject(ObjIdx: Integer);
var
  Ch: Char;
begin
  SetColor(LightRed, Black);
  WriteAt(1, 24, 'Delete "' + World.Objects[ObjIdx].Name + '"? (Y/N) ');
  ResetColor;

  Ch := ReadKey;
  if UpCase(Ch) = 'Y' then
  begin
    World.Objects[ObjIdx].Active := False;
    Modified := True;
  end;
end;

procedure HandleObjectList;
var
  Ch: Char;
  I: Integer;
begin
  SelectedObject := 0;
  { Find first active object }
  for I := 1 to MAX_OBJECTS do
    if World.Objects[I].Active then
    begin
      SelectedObject := I;
      Break;
    end;

  repeat
    DrawObjectList;
    Ch := ReadKey;

    case Ch of
      #0: { Extended key }
        case ReadKey of
          #72: { Up }
            begin
              for I := SelectedObject - 1 downto 1 do
                if World.Objects[I].Active then
                begin
                  SelectedObject := I;
                  Break;
                end;
            end;
          #80: { Down }
            begin
              for I := SelectedObject + 1 to MAX_OBJECTS do
                if World.Objects[I].Active then
                begin
                  SelectedObject := I;
                  Break;
                end;
            end;
        end;
      'e', 'E':
        if SelectedObject > 0 then
          EditObjectForm(SelectedObject, False);
      'd', 'D':
        if SelectedObject > 0 then
          DeleteObject(SelectedObject);
      'a', 'A':
        EditObjectForm(0, True);
      #27: { Escape }
        Exit;
    end;
  until False;
end;

{ Mob editing procedures }

procedure DrawMobList;
var
  I, Y, Count: Integer;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== MOB LIST ===');
  ResetColor;

  Y := 6;
  Count := 0;
  for I := 1 to MAX_MOBS do
  begin
    if World.Mobs[I].Active and (Y < 21) then
    begin
      Inc(Count);
      if I = SelectedMob then
        SetColor(Black, White)
      else
        SetColor(LightGray, Black);

      WriteAt(3, Y, Format('%3d: %-25s Room:%3d',
        [World.Mobs[I].ID,
         World.Mobs[I].Name,
         World.Mobs[I].RoomID]));
      Inc(Y);
      ResetColor;
    end;
  end;

  if Count = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteCenter(12, '(No mobs defined)');
    ResetColor;
  end;

  SetColor(Cyan, Black);
  WriteAt(1, 23, 'Up/Down: Select  E: Edit  D: Delete  A: Add  Esc: Back');
  ResetColor;
end;

procedure EditMobForm(MobIdx: Integer; IsNew: Boolean);
var
  M: TMob;
  S: string;
  Field: Integer;
begin
  if IsNew then
  begin
    InitMob(M);
    M.ID := World.MobCount + 1;
    M.Active := True;
  end
  else
    M := World.Mobs[MobIdx];

  Field := 0;

  repeat
    ClearScreen;
    DrawHeader;

    if IsNew then
      SetColor(Yellow, Black)
    else
      SetColor(LightGreen, Black);

    if IsNew then
      WriteCenter(4, '=== ADD NEW MOB ===')
    else
      WriteCenter(4, '=== EDIT MOB ===');
    ResetColor;

    { Display fields }
    WriteAt(5, 7, 'ID:          ');
    WriteAt(20, 7, IntToStr(M.ID));

    if Field = 0 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 9, 'Name:        ');
    WriteAt(20, 9, M.Name + '                              ');
    ResetColor;

    if Field = 1 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 11, 'Description: ');
    WriteAt(20, 11, M.Desc);
    ResetColor;

    if Field = 2 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 13, 'Room ID:     ');
    WriteAt(20, 13, IntToStr(M.RoomID) + '    ');
    ResetColor;

    if Field = 3 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 15, 'Dialogue:    ');
    WriteAt(20, 15, M.Dialogue);
    ResetColor;

    if Field = 4 then SetColor(Black, White) else SetColor(LightGray, Black);
    WriteAt(5, 17, 'First Talk:  ');
    WriteAt(20, 17, IntToStr(M.FirstTalkPara) + '    ');
    ResetColor;

    SetColor(Cyan, Black);
    WriteAt(1, 19, 'First Talk plays a paragraph the first time the player talks here.');
    WriteAt(1, 20, 'Tab: Next Field  Enter: Edit Field  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 5;
      #13: { Enter - edit current field }
        begin
          case Field of
            0: begin
                 S := ReadLine(20, 9, MAX_OBJ_NAME);
                 if S <> '' then M.Name := S;
               end;
            1: begin
                 S := ReadLine(20, 11, MAX_OBJ_DESC);
                 if S <> '' then M.Desc := S;
               end;
            2: begin
                 S := ReadLine(20, 13, 5);
                 M.RoomID := StrToIntDef(S, M.RoomID);
               end;
            3: begin
                 S := ReadLine(20, 15, MAX_DIALOGUE);
                 if S <> '' then M.Dialogue := S;
               end;
            4: begin
                 S := ReadLine(20, 17, 5);
                 M.FirstTalkPara := StrToIntDef(S, M.FirstTalkPara);
               end;
          end;
        end;
      #0: { Extended key }
        case ReadKey of
          #60: { F2 - Save }
            begin
              if IsNew then
              begin
                Inc(World.MobCount);
                MobIdx := World.MobCount;
              end;
              World.Mobs[MobIdx] := M;
              Modified := True;
              Exit;
            end;
          #72: { Up }
            if Field > 0 then Dec(Field);
          #80: { Down }
            if Field < 4 then Inc(Field);
        end;
      #27: { Escape }
        Exit;
    end;
  until False;
end;

procedure DeleteMob(MobIdx: Integer);
var
  Ch: Char;
begin
  SetColor(LightRed, Black);
  WriteAt(1, 24, 'Delete "' + World.Mobs[MobIdx].Name + '"? (Y/N) ');
  ResetColor;

  Ch := ReadKey;
  if UpCase(Ch) = 'Y' then
  begin
    World.Mobs[MobIdx].Active := False;
    Modified := True;
  end;
end;

procedure HandleMobList;
var
  Ch: Char;
  I: Integer;
begin
  SelectedMob := 0;
  { Find first active mob }
  for I := 1 to MAX_MOBS do
    if World.Mobs[I].Active then
    begin
      SelectedMob := I;
      Break;
    end;

  repeat
    DrawMobList;
    Ch := ReadKey;

    case Ch of
      #0: { Extended key }
        case ReadKey of
          #72: { Up }
            begin
              for I := SelectedMob - 1 downto 1 do
                if World.Mobs[I].Active then
                begin
                  SelectedMob := I;
                  Break;
                end;
            end;
          #80: { Down }
            begin
              for I := SelectedMob + 1 to MAX_MOBS do
                if World.Mobs[I].Active then
                begin
                  SelectedMob := I;
                  Break;
                end;
            end;
        end;
      'e', 'E':
        if SelectedMob > 0 then
          EditMobForm(SelectedMob, False);
      'd', 'D':
        if SelectedMob > 0 then
          DeleteMob(SelectedMob);
      'a', 'A':
        EditMobForm(0, True);
      #27: { Escape }
        Exit;
    end;
  until False;
end;

{ Splits a stored paragraph into the editing grid, and joins it back. The
  editor has no multi-line control, so a paragraph is edited as N rows of
  ReadLine and stored with #13#10 between them. }
procedure ParaToLines(const S: TParaText; var Lines: TParaLines);
var
  I, Row, Start: Integer;
begin
  for I := 1 to MAX_PARA_LINES do
    Lines[I] := '';
  Row := 1;
  Start := 1;
  I := 1;
  while (I <= Length(S)) and (Row <= MAX_PARA_LINES) do
  begin
    if (S[I] = #13) or (S[I] = #10) then
    begin
      Lines[Row] := Copy(S, Start, I - Start);
      if (S[I] = #13) and (I < Length(S)) and (S[I + 1] = #10) then Inc(I);
      Start := I + 1;
      Inc(Row);
    end;
    Inc(I);
  end;
  if (Start <= Length(S)) and (Row <= MAX_PARA_LINES) then
    Lines[Row] := Copy(S, Start, Length(S) - Start + 1);
end;

function LinesToPara(const Lines: TParaLines): TParaText;
var
  I, Last: Integer;
begin
  Last := 0;
  for I := 1 to MAX_PARA_LINES do
    if Lines[I] <> '' then Last := I;
  Result := '';
  for I := 1 to Last do
  begin
    if I > 1 then Result := Result + #13#10;
    Result := Result + Lines[I];
  end;
end;

procedure EditParagraphForm(Num: Integer);
var
  Lines: TParaLines;
  Row, I: Integer;
begin
  ParaToLines(World.Paragraphs[Num], Lines);
  Row := 1;

  repeat
    ClearScreen;
    DrawHeader;

    SetColor(Yellow, Black);
    WriteCenter(3, '=== EDIT PARAGRAPH ' + IntToStr(Num) + ' ===');
    ResetColor;

    for I := 1 to MAX_PARA_LINES do
    begin
      if I = Row then SetColor(Black, White) else SetColor(LightGray, Black);
      WriteAt(2, 3 + I, Copy(Lines[I] + StringOfChar(' ', PARA_COLS), 1, PARA_COLS));
      ResetColor;
    end;

    SetColor(Cyan, Black);
    WriteAt(1, 24, 'Up/Down: Line  Enter: Edit  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #13:
        Lines[Row] := ReadLine(2, 3 + Row, PARA_COLS);
      #9:
        Row := (Row mod MAX_PARA_LINES) + 1;
      #0:
        case ReadKey of
          #60: { F2 - Save }
            begin
              SetParagraph(World, Num, LinesToPara(Lines));
              Modified := True;
              Exit;
            end;
          #72: if Row > 1 then Dec(Row);
          #80: if Row < MAX_PARA_LINES then Inc(Row);
        end;
      #27:
        Exit;
    end;
  until False;
end;

procedure DrawParagraphList;
var
  I, J, Y, Shown: Integer;
  Preview: string;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== STORY PARAGRAPHS ===');
  ResetColor;

  Shown := 0;
  for I := 1 to MAX_PARAGRAPHS do
  begin
    Y := 6 + Shown;
    if (World.Paragraphs[I] <> '') and (Y < 21) then
    begin
      Inc(Shown);
      if I = SelectedPara then
        SetColor(Black, White)
      else
        SetColor(LightGray, Black);
      { Show the opening words so a paragraph is recognisable in the list }
      Preview := Copy(World.Paragraphs[I], 1, 60);
      for J := 1 to Length(Preview) do
        if (Preview[J] = #13) or (Preview[J] = #10) then Preview[J] := ' ';
      WriteAt(3, Y, Copy('  ' + IntToStr(I) + '. ' + Preview +
                         StringOfChar(' ', 70), 1, 72));
      ResetColor;
    end;
  end;

  if Shown = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteAt(3, 6, 'No paragraphs yet. Press A to write one.');
    ResetColor;
  end;

  SetColor(Cyan, Black);
  WriteAt(1, 22, 'Numbers are printed in the booklet, so deleting leaves a gap.');
  WriteAt(1, 23, 'Up/Dn Select  E Edit  A Add  D Del  X Booklet  R Xref  Esc');
  ResetColor;
end;

procedure ExportBooklet;
var
  F: Text;
  FileName: string;
  I: Integer;
begin
  SetColor(Cyan, Black);
  WriteAt(1, 24, 'Booklet file [ORBLORE.TXT]: ');
  ResetColor;
  FileName := ReadLine(29, 24, 40);
  if FileName = '' then FileName := 'ORBLORE.TXT';

  {$I-}
  Assign(F, FileName);
  Rewrite(F);
  {$I+}
  if IOResult <> 0 then
  begin
    SetColor(LightRed, Black);
    WriteAt(1, 24, 'Could not write ' + FileName + '. Press any key...        ');
    ResetColor;
    ReadKey;
    Exit;
  end;

  WriteLn(F, World.Title);
  WriteLn(F, StringOfChar('=', Length(World.Title)));
  WriteLn(F);
  WriteLn(F, 'Do not read ahead. Read each paragraph only when the game');
  WriteLn(F, 'tells you to.');
  WriteLn(F);

  for I := 1 to World.ParaCount do
    if World.Paragraphs[I] <> '' then
    begin
      WriteLn(F, '--- ', I, ' ---');
      WriteParaBody(F, World.Paragraphs[I]);
      WriteLn(F);
    end;

  Close(F);

  SetColor(LightGreen, Black);
  WriteAt(1, 24, 'Wrote ' + FileName + '. Press any key...              ');
  ResetColor;
  ReadKey;
end;

{ The author's companion to the booklet. Kept as a separate file because the
  booklet is what the player is handed, and a list of what fires when would
  spoil it. }
procedure ExportXRef;
var
  FileName: string;
begin
  SetColor(Cyan, Black);
  WriteAt(1, 24, 'Cross-reference file [ORBXREF.TXT]: ');
  ResetColor;
  FileName := ReadLine(37, 24, 40);
  if FileName = '' then FileName := 'ORBXREF.TXT';

  if WriteParaXRef(FileName, World) then
  begin
    SetColor(LightGreen, Black);
    WriteAt(1, 24, 'Wrote ' + FileName + '. Press any key...              ');
  end
  else
  begin
    SetColor(LightRed, Black);
    WriteAt(1, 24, 'Could not write ' + FileName + '. Press any key...    ');
  end;
  ResetColor;
  ReadKey;
end;

procedure HandleParagraphList;
var
  Ch: Char;
  I: Integer;
  S: string;
begin
  SelectedPara := 0;
  for I := 1 to MAX_PARAGRAPHS do
    if World.Paragraphs[I] <> '' then
    begin
      SelectedPara := I;
      Break;
    end;

  repeat
    DrawParagraphList;
    Ch := ReadKey;

    case UpCase(Ch) of
      #0:
        case ReadKey of
          #72: { Up }
            for I := SelectedPara - 1 downto 1 do
              if World.Paragraphs[I] <> '' then
              begin
                SelectedPara := I;
                Break;
              end;
          #80: { Down }
            for I := SelectedPara + 1 to MAX_PARAGRAPHS do
              if World.Paragraphs[I] <> '' then
              begin
                SelectedPara := I;
                Break;
              end;
        end;
      'E':
        if SelectedPara > 0 then
          EditParagraphForm(SelectedPara);
      'A':
        begin
          SetColor(Cyan, Black);
          WriteAt(1, 24, 'Paragraph number (1-' + IntToStr(MAX_PARAGRAPHS) +
                         '): ');
          ResetColor;
          S := ReadLine(26, 24, 4);
          I := StrToIntDef(S, 0);
          if (I >= 1) and (I <= MAX_PARAGRAPHS) then
          begin
            SelectedPara := I;
            EditParagraphForm(I);
          end;
        end;
      'D':
        if SelectedPara > 0 then
        begin
          SetColor(LightRed, Black);
          WriteAt(1, 24, 'Delete paragraph ' + IntToStr(SelectedPara) +
                         '? Numbering will keep the gap. (Y/N) ');
          ResetColor;
          if UpCase(ReadKey) = 'Y' then
          begin
            SetParagraph(World, SelectedPara, '');
            Modified := True;
            SelectedPara := 0;
            for I := 1 to MAX_PARAGRAPHS do
              if World.Paragraphs[I] <> '' then
              begin
                SelectedPara := I;
                Break;
              end;
          end;
        end;
      'X':
        ExportBooklet;
      'R':
        ExportXRef;
      #27:
        Exit;
    end;
  until False;
end;

{ ---- Events ------------------------------------------------------------

  This editor reads events; it does not write them. Authoring a condition
  and action list needs more screen and more control than a 25-row CRT form
  has, and this is the editor that ships on the floppy beside the game -
  editor-tv and the browser editor have the room for it and no size budget.
  What is here is what an author needs while playing with a world on a DOS
  box: see what events exist, turn one off to bisect a misbehaving world, and
  delete one outright. }

{ The highest slot still in use, which is what EventCount means and what the
  loader would compute on the next read. Kept in step after a delete so that
  saving and reloading a world is idempotent. }
procedure RecomputeEventCount;
var
  I: Integer;
begin
  World.EventCount := 0;
  for I := MAX_EVENTS downto 1 do
    if World.Events[I].Active then
    begin
      World.EventCount := I;
      Break;
    end;
end;

{ "room 5", "object 12" - whatever the trigger's IDs mean for this trigger.
  Reading a raw pair of numbers off the screen and guessing which is which is
  most of what makes a hand-written event hard to check. }
function TriggerDetail(const E: TWorldEvent): string;

  function Any(ID: Word; const What: string): string;
  begin
    if ID = 0 then Result := 'any ' + What
    else Result := What + ' ' + IntToStr(ID);
  end;

begin
  case E.TriggerType of
    etEnterRoom, etFirstVisit:
      Result := Any(E.TriggerID, 'room');
    etExitRoom:
      begin
        Result := Any(E.TriggerID, 'room');
        if E.TriggerID2 > 0 then
          Result := Result + ' -> room ' + IntToStr(E.TriggerID2);
      end;
    etTakeObject, etDropObject, etUseObject, etExamineObject:
      Result := Any(E.TriggerID, 'object');
    etUseObjectOn:
      Result := Any(E.TriggerID, 'object') + ' on ' +
                Any(E.TriggerID2, 'object');
    etTalkToMob:
      Result := Any(E.TriggerID, 'mob');
    etGiveTo:
      Result := Any(E.TriggerID, 'object') + ' to ' + Any(E.TriggerID2, 'mob');
    etTimer:
      begin
        Result := 'turn ' + IntToStr(E.TriggerID);
        if E.TriggerID2 > 0 then
          Result := Result + ', then every ' + IntToStr(E.TriggerID2)
        else
          Result := Result + ' only';
      end;
    etFlagSet, etFlagClear:
      Result := Any(E.TriggerID, 'flag');
  else
    Result := '';
  end;
end;

function ConditionLine(const C: TCondition): string;
begin
  Result := ConditionName(C.CondType) + ' ' + IntToStr(C.TargetID);
  if C.Value <> 0 then Result := Result + ', ' + IntToStr(C.Value);
  if C.Negate then Result := Result + '  (NOT)';
end;

function ActionLine(const A: TAction): string;
begin
  Result := ActionName(A.ActionType) + ' ' + IntToStr(A.TargetID);
  if A.Value <> 0 then Result := Result + ', ' + IntToStr(A.Value);
  if A.Text <> '' then Result := Result + '  "' + A.Text + '"';
end;

procedure ViewEventForm(Slot: Integer);
var
  I, Y: Integer;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== EVENT ' + IntToStr(Slot) + ' ===');
  ResetColor;

  Y := 6;
  WriteAt(3, Y, 'Name:    ' + World.Events[Slot].Name);
  Inc(Y);
  WriteAt(3, Y, 'Trigger: ' + TriggerName(World.Events[Slot].TriggerType) +
                '  (' + TriggerDetail(World.Events[Slot]) + ')');
  Inc(Y);
  if World.Events[Slot].OneShot then
    WriteAt(3, Y, 'Fires:   once')
  else
    WriteAt(3, Y, 'Fires:   every time');
  Inc(Y);
  if World.Events[Slot].Enabled then
    WriteAt(3, Y, 'Starts:  enabled')
  else
    WriteAt(3, Y, 'Starts:  disabled');
  Inc(Y, 2);

  SetColor(Cyan, Black);
  WriteAt(3, Y, 'Conditions (all must hold):');
  ResetColor;
  Inc(Y);
  if World.Events[Slot].CondCount = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteAt(5, Y, '(none)');
    ResetColor;
    Inc(Y);
  end
  else
    for I := 1 to World.Events[Slot].CondCount do
    begin
      WriteAt(5, Y, Copy(IntToStr(I) + '. ' +
                         ConditionLine(World.Events[Slot].Conditions[I]),
                         1, 72));
      Inc(Y);
    end;

  Inc(Y);
  SetColor(Cyan, Black);
  WriteAt(3, Y, 'Actions (in order):');
  ResetColor;
  Inc(Y);
  if World.Events[Slot].ActionCount = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteAt(5, Y, '(none)');
    ResetColor;
  end
  else
    for I := 1 to World.Events[Slot].ActionCount do
    begin
      if Y > 21 then Break;
      WriteAt(5, Y, Copy(IntToStr(I) + '. ' +
                         ActionLine(World.Events[Slot].Actions[I]), 1, 72));
      Inc(Y);
    end;

  SetColor(Cyan, Black);
  WriteAt(1, 23, 'Read-only here. Author events in editor-tv or the web ' +
                 'editor.  Esc');
  ResetColor;
  repeat until ReadKey = #27;
end;

procedure DrawEventList;
var
  I, Y, Shown: Integer;
  Line: string;
begin
  ClearScreen;
  DrawHeader;

  SetColor(Yellow, Black);
  WriteCenter(4, '=== EVENTS ===');
  ResetColor;

  Shown := 0;
  for I := 1 to MAX_EVENTS do
  begin
    Y := 6 + Shown;
    if World.Events[I].Active and (Y < 21) then
    begin
      Inc(Shown);
      if I = SelectedEvent then
        SetColor(Black, White)
      else if not World.Events[I].Enabled then
        SetColor(DarkGray, Black)
      else
        SetColor(LightGray, Black);
      Line := '  ' + Copy(IntToStr(I) + '.   ', 1, 5);
      if World.Events[I].Enabled then Line := Line + '[on ] '
                                  else Line := Line + '[off] ';
      Line := Line + Copy(World.Events[I].Name + StringOfChar(' ', 26), 1, 26) +
              TriggerName(World.Events[I].TriggerType);
      WriteAt(3, Y, Copy(Line + StringOfChar(' ', 72), 1, 72));
      ResetColor;
    end;
  end;

  if Shown = 0 then
  begin
    SetColor(DarkGray, Black);
    WriteAt(3, 6, 'No events. Author them in editor-tv or the web editor.');
    ResetColor;
  end;

  SetColor(Cyan, Black);
  WriteAt(1, 22, 'Slot numbers are identity: save games index them, so a ' +
                 'delete leaves a gap.');
  WriteAt(1, 23, 'Up/Dn Select  V View  T Toggle on/off  D Del  Esc');
  ResetColor;
end;

procedure HandleEventList;
var
  Ch: Char;
  I: Integer;
begin
  SelectedEvent := 0;
  for I := 1 to MAX_EVENTS do
    if World.Events[I].Active then
    begin
      SelectedEvent := I;
      Break;
    end;

  repeat
    DrawEventList;
    Ch := ReadKey;

    case UpCase(Ch) of
      #0:
        case ReadKey of
          #72: { Up }
            for I := SelectedEvent - 1 downto 1 do
              if World.Events[I].Active then
              begin
                SelectedEvent := I;
                Break;
              end;
          #80: { Down }
            for I := SelectedEvent + 1 to MAX_EVENTS do
              if World.Events[I].Active then
              begin
                SelectedEvent := I;
                Break;
              end;
        end;
      'V':
        if SelectedEvent > 0 then
          ViewEventForm(SelectedEvent);
      'T':
        if SelectedEvent > 0 then
        begin
          { Enabled is the authored starting state. The live EvEnabled bitmap
            is reseeded from it, because nothing has started playing yet. }
          World.Events[SelectedEvent].Enabled :=
            not World.Events[SelectedEvent].Enabled;
          SeedEventState(World);
          Modified := True;
        end;
      'D':
        if SelectedEvent > 0 then
        begin
          SetColor(LightRed, Black);
          WriteAt(1, 24, 'Delete event ' + IntToStr(SelectedEvent) +
                         '? Slot ' + IntToStr(SelectedEvent) +
                         ' stays empty. (Y/N) ');
          ResetColor;
          if UpCase(ReadKey) = 'Y' then
          begin
            InitEvent(World.Events[SelectedEvent]);
            RecomputeEventCount;
            SeedEventState(World);
            Modified := True;
            SelectedEvent := 0;
            for I := 1 to MAX_EVENTS do
              if World.Events[I].Active then
              begin
                SelectedEvent := I;
                Break;
              end;
          end;
        end;
      #27:
        Exit;
    end;
  until False;
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
      '3': HandleObjectList;
      '4': EditObjectForm(0, True);
      '5': HandleMobList;
      '6': EditMobForm(0, True);
      'P': HandleParagraphList;
      'E': HandleEventList;
      'V': ValidateForm;
      '7': WorldSettings;
      '8': LoadWorldFile;
      '9': SaveWorldFile;
      '0': NewWorld;
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
  SelectedRoom := 0;
  SelectedObject := 0;
  SelectedMob := 0;
  SelectedPara := 0;
  SelectedEvent := 0;

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
