program mansion;

uses
  SysUtils, GameConfig, Rooms;

{ Main Game Loop }
procedure MainGameLoop;
var choice: string;
begin
  loadIni;
  repeat
    WriteLn;
    WriteLn('1) Add Room');
    WriteLn('2) List all Rooms');
    WriteLn('3) Check specific Room');
    WriteLn('4) Quit Program');
    WriteLn('Option: ');
    ReadLn(choice);

    case choice of
      '1': AddRoom;
      '2': ListAllRooms;
      '3': ShowRoom;
      '4': WriteLn('Exiting');
    else
      WriteLn('Invalid option');
    end;
  until choice = '4';
end;

begin
  InitializeGameFile;
  WriteLn('Welcome to Mansion');
  WriteLn('====================');
  MainGameLoop;
end.

