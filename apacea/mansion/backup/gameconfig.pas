unit GameConfig;

{ Simple INI file handling - TP7 compatible }

interface

{ Global configuration variables }
var
  OutputFileName: string;
  MapFileName: string;
  ScriptFileName: string;
  OutputPath: string;
  SourcePath: string;
  MaxStringLength: Integer;
  DebugMode: Boolean;
  CompressData: Boolean;

{ Simple procedures to load and save config }
procedure LoadConfig;
procedure SaveConfig;
procedure SetDefaults;
procedure loadIni;

implementation

procedure loadIni;
var
  StringLength: integer;
  OutputFile: string;
begin
  WriteLn('Game Builder Tool v1.0');
  WriteLn('======================');

  { Load configuration from BUILDER.INI }
  LoadConfig;

  WriteLn('Loaded configuration from BUILDER.INI');
  WriteLn;
  WriteLn('Current settings:');
  WriteLn('  Max String Length: ', MaxStringLength, ' bytes');
  WriteLn('  Output File: ', OutputFileName);
  WriteLn('  Output Path: ', OutputPath);
  WriteLn('  Debug Mode: ', DebugMode);
  WriteLn;

end;

procedure SetDefaults;
begin
  OutputFileName := 'GAME.DAT';
  MapFileName := 'MAPS.DAT';
  ScriptFileName := 'SCRIPT.DAT';
  OutputPath := '.\';
  SourcePath := '.\SRC\';
  MaxStringLength := 80;
  DebugMode := False;
  CompressData := True;
end;

procedure LoadConfig;
var
  ConfigFile: Text;
  Line, Key, Value: string;
  EqualPos: Integer;
  ErrorCode: Integer;
begin
  { Set defaults first }
  SetDefaults;

  { Try to open config file }
  Assign(ConfigFile, 'BUILDER.INI');
  {$I-} Reset(ConfigFile); {$I+}
  if IOResult <> 0 then
  begin
    { File doesn't exist, create it }
    SaveConfig;
    Exit;
  end;

  { Read each line and parse Key=Value pairs }
  while not Eof(ConfigFile) do
  begin
    Readln(ConfigFile, Line);

    { Skip empty lines and comments }
    if (Length(Line) = 0) or (Line[1] = ';') or (Line[1] = '[') then
      Continue;

    { Find the = sign }
    EqualPos := Pos('=', Line);
    if EqualPos > 1 then
    begin
      Key := Copy(Line, 1, EqualPos - 1);
      Value := Copy(Line, EqualPos + 1, Length(Line) - EqualPos);

      { Set the appropriate variable }
      if Key = 'OutputFile' then OutputFileName := Value
      else if Key = 'MapFile' then MapFileName := Value
      else if Key = 'ScriptFile' then ScriptFileName := Value
      else if Key = 'OutputPath' then OutputPath := Value
      else if Key = 'SourcePath' then SourcePath := Value
      else if Key = 'MaxStringLength' then
      begin
        Val(Value, MaxStringLength, ErrorCode);
        if (ErrorCode <> 0) or (MaxStringLength < 40) or (MaxStringLength > 255) then
          MaxStringLength := 80;
      end
      else if Key = 'DebugMode' then
        DebugMode := (Value = 'True') or (Value = 'true')
      else if Key = 'CompressData' then
        CompressData := (Value = 'True') or (Value = 'true');
    end;
  end;

  Close(ConfigFile);

  { Ensure paths end with backslash }
  if (Length(OutputPath) > 0) and (OutputPath[Length(OutputPath)] <> '\') then
    OutputPath := OutputPath + '\';
  if (Length(SourcePath) > 0) and (SourcePath[Length(SourcePath)] <> '\') then
    SourcePath := SourcePath + '\';
end;

procedure SaveConfig;
var
  ConfigFile: Text;
begin
  Assign(ConfigFile, 'BUILDER.INI');
  Rewrite(ConfigFile);

  Writeln(ConfigFile, '; Game Builder Configuration');
  Writeln(ConfigFile, '');
  Writeln(ConfigFile, '[Files]');
  Writeln(ConfigFile, 'OutputFile=', OutputFileName);
  Writeln(ConfigFile, 'MapFile=', MapFileName);
  Writeln(ConfigFile, 'ScriptFile=', ScriptFileName);
  Writeln(ConfigFile, '');
  Writeln(ConfigFile, '[Paths]');
  Writeln(ConfigFile, 'OutputPath=', OutputPath);
  Writeln(ConfigFile, 'SourcePath=', SourcePath);
  Writeln(ConfigFile, '');
  Writeln(ConfigFile, '[Settings]');
  Writeln(ConfigFile, 'MaxStringLength=', MaxStringLength);
  if DebugMode then
    Writeln(ConfigFile, 'DebugMode=True')
  else
    Writeln(ConfigFile, 'DebugMode=False');
  if CompressData then
    Writeln(ConfigFile, 'CompressData=True')
  else
    Writeln(ConfigFile, 'CompressData=False');

  Close(ConfigFile);
end;

end.
