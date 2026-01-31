program DOSApp;

{$IFDEF MSDOS}
  {$MODE TP} { Turbo Pascal compatibility mode for DOS }
{$ELSE}
  {$MODE FPC}
{$ENDIF}

uses
  {$IFDEF MSDOS}DOS, CRT{$ELSE}SysUtils{$ENDIF};

var
  s: {$IFDEF MSDOS}String[255]{$ELSE}String{$ENDIF};

begin
  {$IFDEF MSDOS}
    ClrScr;
    WriteLn('Running on DOS!');
    WriteLn('Free memory: ', MemAvail, ' bytes');
  {$ELSE}
    WriteLn('Running on modern OS');
  {$ENDIF}
  
  WriteLn('Hello from Free Pascal!');
  ReadLn;
end.
