{ bpldump.pas - Load a BPL world and print what the parser made of it }
{ A development aid: it is the only way to see resolved exits and parse
  errors without stepping through the editor. Not part of the release. }
program BPLDump;

{$MODE OBJFPC}

uses
  SysUtils, GameData, BPLPars;

var
  World: TGameWorld;
  Errors: TBPLErrorArray;
  ErrCount, I: Integer;
  D: TDirection;
  Line: string;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: bpldump <world.bpl>');
    Halt(1);
  end;

  if LoadWorldBPL(ParamStr(1), World) then
    WriteLn('LOAD: ok')
  else
    WriteLn('LOAD: failed');

  if GetBPLErrors(Errors, ErrCount) then
    for I := 1 to ErrCount do
      WriteLn('ERROR line ', Errors[I].Line, ': ', Errors[I].Message)
  else
    WriteLn('ERRORS: none');

  WriteLn('TITLE: ', World.Title);
  WriteLn('START: ', World.CurrentRoom);
  WriteLn('COUNTS: rooms=', World.RoomCount,
          ' objects=', World.ObjectCount, ' mobs=', World.MobCount);

  for I := 1 to World.RoomCount do
    if World.Rooms[I].Active then
    begin
      Line := 'ROOM ' + IntToStr(World.Rooms[I].ID) + ' "' +
              World.Rooms[I].Name + '" exits:';
      for D := Low(TDirection) to High(TDirection) do
        Line := Line + ' ' + GetExitName(D) + '=' +
                IntToStr(World.Rooms[I].Exits[D]);
      WriteLn(Line);
    end;

  for I := 1 to World.ObjectCount do
    if World.Objects[I].Active then
      WriteLn('OBJECT ', World.Objects[I].ID, ' "', World.Objects[I].Name,
              '" room=', World.Objects[I].RoomID,
              ' carriedby=', World.Objects[I].CarriedBy);

  for I := 1 to World.MobCount do
    if World.Mobs[I].Active then
      WriteLn('MOB ', World.Mobs[I].ID, ' "', World.Mobs[I].Name,
              '" room=', World.Mobs[I].RoomID);
end.
