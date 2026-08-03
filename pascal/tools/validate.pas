{ validate.pas - Run the editor's world checks from the command line }
{ A development aid and a way to check a world in CI. Not part of the
  release: the checks belong to the authoring side, not the game. }
program Validate;

{$MODE OBJFPC}

uses
  SysUtils, GameData, DataFile, WorldVal;

var
  World: TGameWorld;
  Issues: TIssueList;
  Count, I, Errors: Integer;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: validate <world file>');
    Halt(1);
  end;

  if not LoadWorld(ParamStr(1), World) then
  begin
    WriteLn('Could not load ', ParamStr(1));
    Halt(2);
  end;

  Count := ValidateWorld(World, Issues);
  Errors := 0;

  for I := 1 to Count do
  begin
    if Issues[I].Level = ilError then Inc(Errors);
    WriteLn(IssueLevelName(Issues[I].Level), '  [', Issues[I].Where, '] ',
            Issues[I].Text);
  end;

  WriteLn(Count, ' issue(s): ', Errors, ' error(s), ',
          Count - Errors, ' warning(s)');
  if Errors > 0 then Halt(1);
end.
