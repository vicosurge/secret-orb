{ converter.pas - Convert text format worlds to binary format }
program WorldConverter;

{$MODE OBJFPC}

uses
  SysUtils, GameData, DataFile;

var
  World: TGameWorld;
  InputFile, OutputFile: string;

begin
  WriteLn('Secret Orb World Format Converter');
  WriteLn('Text Format -> Binary Format');
  WriteLn;

  if ParamCount < 2 then
  begin
    WriteLn('Usage: converter <input.dat> <output.dat>');
    WriteLn;
    WriteLn('Example: converter world.txt world.bin');
    Halt(1);
  end;

  InputFile := ParamStr(1);
  OutputFile := ParamStr(2);

  WriteLn('Loading: ', InputFile);
  if not LoadWorld(InputFile, World) then
  begin
    WriteLn('Error: Failed to load input file');
    Halt(2);
  end;

  WriteLn('Loaded ', World.RoomCount, ' rooms, ',
          World.ObjectCount, ' objects, ',
          World.MobCount, ' mobs');

  WriteLn('Saving: ', OutputFile);
  if not SaveWorldBinary(OutputFile, World) then
  begin
    WriteLn('Error: Failed to save output file');
    Halt(3);
  end;

  WriteLn('Conversion complete!');
  WriteLn;
  WriteLn('You can now use ', OutputFile, ' with Secret Orb');
end.
