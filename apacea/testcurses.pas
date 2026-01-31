program TestNCurses;
uses ncurses;

begin
  initscr;
  mvprintw(10, 10, 'Hello TUI World!');
  refresh;
  getch;
  endwin;
end.
