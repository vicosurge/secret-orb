program soundtest;
uses CRT;

var
  i: integer;  { Need to declare this too! }

{ Define the Beep procedure }
procedure Beep(freq, duration: word);
begin
  Sound(freq);
  Delay(duration);
  NoSound;
end;

procedure RoomSound(roomType: string);
begin
  if roomType = 'cave' then
  begin
    { Echo effect }
    Beep(200, 100);
    Delay(50);
    Beep(200, 50);  { Quieter echo }
    Delay(100);
    Beep(200, 25);  { Even quieter }
  end
  else if roomType = 'underwater' then
  begin
    { Bubbling sound }
    for i := 1 to 10 do
    begin
      Sound(Random(100) + 100);
      Delay(30);
    end;
    NoSound;
  end
  else if roomType = 'forest' then
  begin
    { Bird chirps }
    Beep(2000, 30);
    Beep(2200, 30);
    Beep(2000, 30);
    Delay(500);
    Beep(1800, 50);
  end;
end;

procedure PlayJingle(jingleType: string);
begin
  if jingleType = 'treasure' then
  begin
    { Ascending fanfare }
    Beep(262, 100);  { C }
    Beep(330, 100);  { E }
    Beep(392, 100);  { G }
    Beep(523, 300);  { C5 }
  end
  else if jingleType = 'danger' then
  begin
    { Jaws-like }
    Beep(100, 200);
    Delay(100);
    Beep(90, 200);
    Delay(50);
    Beep(100, 100);
    Beep(90, 100);
  end
  else if jingleType = 'victory' then
  begin
    { Final Fantasy style }
    Beep(440, 150);
    Beep(440, 150);
    Beep(440, 150);
    Beep(440, 400);
    Beep(349, 300);
    Beep(392, 300);
    Beep(440, 600);
  end;
end;

begin
  Randomize;  { Initialize random number generator }

  writeln('Testing room ambience...');
  RoomSound('cave');
  Delay(1000);

  writeln('Underwater sounds...');
  RoomSound('underwater');
  Delay(1000);

  writeln('Forest sounds...');
  RoomSound('forest');
  Delay(1000);

  writeln('You found treasure!');
  PlayJingle('treasure');
  Delay(1000);

  writeln('Enemy approaches!');
  PlayJingle('danger');
  Delay(1000);

  writeln('Victory!');
  PlayJingle('victory');
end.
