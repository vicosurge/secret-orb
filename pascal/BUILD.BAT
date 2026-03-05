@ECHO OFF
REM Secret Orb - FreeDOS Build Script
REM Usage: BUILD [ALL|GAME|EDITOR|CLEAN]
REM
REM If FPC is not on your PATH, set the full path here:
REM   SET FPC=C:\FPC\BIN\FPC.EXE

SET FPC=fpc
SET SRCDIR=src
SET BINDIR=bin
SET DATADIR=data
SET FPCFLAGS=-O2 -XX -CX -Xs -Fu%SRCDIR%

IF x%1==x        GOTO build_all
IF x%1==xALL     GOTO build_all
IF x%1==xall     GOTO build_all
IF x%1==xGAME    GOTO build_game
IF x%1==xgame    GOTO build_game
IF x%1==xEDITOR  GOTO build_editor
IF x%1==xeditor  GOTO build_editor
IF x%1==xCLEAN   GOTO clean
IF x%1==xclean   GOTO clean
ECHO Unknown target: %1
ECHO Usage: BUILD ALL, GAME, EDITOR or CLEAN
GOTO end

:build_all
IF NOT EXIST %BINDIR%\NUL MD %BINDIR%
ECHO Building secorb.exe...
%FPC% %FPCFLAGS% -o%BINDIR%\secorb.exe secorb.pas
IF ERRORLEVEL 1 GOTO error
ECHO Building editor.exe...
%FPC% %FPCFLAGS% -o%BINDIR%\editor.exe editor.pas
IF ERRORLEVEL 1 GOTO error
COPY %DATADIR%\world.dat %BINDIR%\ > NUL
ECHO.
ECHO Build complete. Binaries in %BINDIR%\
GOTO end

:build_game
IF NOT EXIST %BINDIR%\NUL MD %BINDIR%
ECHO Building secorb.exe...
%FPC% %FPCFLAGS% -o%BINDIR%\secorb.exe secorb.pas
IF ERRORLEVEL 1 GOTO error
COPY %DATADIR%\world.dat %BINDIR%\ > NUL
ECHO Done.
GOTO end

:build_editor
IF NOT EXIST %BINDIR%\NUL MD %BINDIR%
ECHO Building editor.exe...
%FPC% %FPCFLAGS% -o%BINDIR%\editor.exe editor.pas
IF ERRORLEVEL 1 GOTO error
ECHO Done.
GOTO end

:clean
ECHO Cleaning build artifacts...
IF EXIST %BINDIR%\*.exe DEL %BINDIR%\*.exe
IF EXIST %BINDIR%\*.dat DEL %BINDIR%\*.dat
IF EXIST %SRCDIR%\*.o   DEL %SRCDIR%\*.o
IF EXIST %SRCDIR%\*.ppu DEL %SRCDIR%\*.ppu
IF EXIST *.o            DEL *.o
IF EXIST *.ppu          DEL *.ppu
ECHO Clean complete.
GOTO end

:error
ECHO.
ECHO ERROR: Build failed. See compiler output above.

:end
