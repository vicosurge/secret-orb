program SimpleTUI;
{$mode objfpc}
uses FV, Objects, Drivers, Views, Menus, Dialogs, App;

type
  TMyApp = object(TApplication)
    procedure InitStatusLine; virtual;
  end;

procedure TMyApp.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      nil), nil)
  ));
end;

var
  MyApp: TMyApp;
begin
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
end.
