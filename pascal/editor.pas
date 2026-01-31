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
  TEditorState = (esMenu, esRoomList, esAddRoom, esEditRoom, esWorldSettings,
                  esObjectList, esAddObject, esEditObject,
                  esMobList, esAddMob, esEditMob);

var
  World: TGameWorld;
  EditorState: TEditorState;
  CurrentFile: string;
  Modified: Boolean;
  SelectedRoom: Integer;
  SelectedObject: Integer;
  SelectedMob: Integer;

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

    SetColor(Cyan, Black);
    WriteAt(1, 20, 'Tab: Next Field  Enter: Edit Field  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 8;
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
            if Field < 7 then Inc(Field);
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

    SetColor(Cyan, Black);
    WriteAt(1, 18, 'Tab/Arrows: Navigate  Enter: Edit/Toggle  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 8;
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
            if Field < 7 then Inc(Field);
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

    SetColor(Cyan, Black);
    WriteAt(1, 20, 'Tab: Next Field  Enter: Edit Field  F2: Save  Esc: Cancel');
    ResetColor;

    case ReadKey of
      #9: { Tab }
        Field := (Field + 1) mod 4;
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
            if Field < 3 then Inc(Field);
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
