# Secret Orb - Makefile for Free Pascal
# Targets: native (default), dos, win32

FPC = fpc
SRCDIR = src
BINDIR = bin
DATADIR = data

# Compiler flags
# -O2: Optimization level 2
# -XX: Smart linking
# -CX: Create smartlinkable units
# -Xs: Strip symbols
FPCFLAGS = -O2 -XX -CX -Xs -Fu$(SRCDIR)

# Target-specific flags
DOS_FLAGS = -Twin16 -WD
WIN32_FLAGS = -Twin32

# DOS 32-bit DPMI (go32v2). No distribution packages a go32v2 cross-compiler,
# so dos/bootstrap-toolchain.sh builds one once into a cache directory and
# everything below resolves through it. -Tgo32v2 and the binutils prefix are
# baked into the fpc-go32v2 wrapper the bootstrap writes.
DOSDIR   = $(BINDIR)/dos
DOSDIST  = dist/SECORB
DOSTOOLS = $(shell dos/bootstrap-toolchain.sh --print-prefix)
DOSFPC   = $(DOSTOOLS)/bin/fpc-go32v2
CWSDPMI  = $(DOSTOOLS)/share/cwsdpmi

# go32v2 unit files must not land next to the native ones: same names, other
# architecture. Keeping them under bin/dos/units means switching between
# "make native" and "make dos32" never needs a clean in between.
DOS32_FLAGS = -O2 -XX -CX -Xs -Fu$(SRCDIR) -FU$(DOSDIR)/units

.PHONY: all clean native dos dos32 win32 release editors editor-tv \
        converter tools test sizecheck \
        dos-toolchain dos-dist dos-test dos-clean

all: native

$(BINDIR):
	mkdir -p $(BINDIR)

# Native build (current platform)
native: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/secretorb secretorb.pas
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/editor editor.pas
	cp $(DATADIR)/world.dat $(BINDIR)/

# Converter tool
converter: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/converter tools/converter.pas

# Development tools. None of these ship: worldval.pas is editor-side only and
# must never be linked into the game.
tools: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/converter tools/converter.pas
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/validate tools/validate.pas
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/bpldump tools/bpldump.pas

# Unit tests. Both write their scratch files into the working directory and
# delete them again, so they need no fixtures.
test: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/pairtest tools/pairtest.pas
	$(BINDIR)/pairtest
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/eventtest tools/eventtest.pas
	$(BINDIR)/eventtest

# Turbo Vision editor (for game creators, not part of player distribution)
editor-tv: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/editor-tv editor-tv.pas

# Build both editors
editors: $(BINDIR)
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/editor editor.pas
	$(FPC) $(FPCFLAGS) -o$(BINDIR)/editor-tv editor-tv.pas

# DOS 16-bit (requires DOS target support in FPC)
dos: $(BINDIR)
	$(FPC) $(FPCFLAGS) $(DOS_FLAGS) -o$(BINDIR)/SECORB.EXE secretorb.pas
	$(FPC) $(FPCFLAGS) $(DOS_FLAGS) -o$(BINDIR)/EDITOR.EXE editor.pas

# DOS 32-bit DPMI (go32v2)
#
# The game is built from secorb.pas, the 8.3-named duplicate of secretorb.pas
# that BUILD.BAT compiles on DOS - CI diffs the two, so they cannot drift. The
# three console tools are not part of the distribution; they are what
# dos-test runs inside FreeDOS, where redirected output can be captured.
# editor-tv is deliberately absent: its Turbo Vision dependency has no place
# in a DOS build.
dos32: dos-toolchain
	mkdir -p $(DOSDIR)/units
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/SECORB.EXE   secorb.pas
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/EDITOR.EXE   editor.pas
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/VALIDATE.EXE tools/validate.pas
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/CONVERT.EXE  tools/converter.pas
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/PAIRTEST.EXE tools/pairtest.pas
	$(DOSFPC) $(DOS32_FLAGS) -o$(DOSDIR)/EVENTTST.EXE tools/eventtest.pas
	cp $(DATADIR)/world.dat $(DOSDIR)/WORLD.DAT
	cp $(CWSDPMI)/CWSDPMI.EXE $(DOSDIR)/
	@echo "DOS binaries in $(DOSDIR)/"

# Builds the cross-compiler if it is missing, and says so rather than letting
# the compile fail with a confusing "no such file".
dos-toolchain:
	@test -x $(DOSFPC) || { \
	  echo "The go32v2 cross-compiler is not built yet."; \
	  echo "Run:  dos/bootstrap-toolchain.sh"; \
	  exit 1; }

# Boot the DOS binaries on real FreeDOS under QEMU and check their output.
# The native tools are prerequisites, not conveniences: the harness compares
# the DOS output against them.
dos-test: dos32 tools test
	dos/run-dos-tests.sh

# DOS release package, plus a real 720KB floppy image. The image is the size
# limit expressed as a build step: if the distribution outgrows the disk the
# project is named after, mcopy fails and so does the build.
dos-dist: dos32
	rm -rf $(DOSDIST)
	mkdir -p $(DOSDIST)
	cp $(DOSDIR)/SECORB.EXE $(DOSDIR)/EDITOR.EXE $(DOSDIST)/
	cp $(DOSDIR)/WORLD.DAT $(DOSDIST)/
	cp $(CWSDPMI)/CWSDPMI.EXE $(DOSDIST)/
	# DOS wants CRLF. Strip before adding, so a file that already has it
	# (cwsdpmi.doc does) does not come out with doubled carriage returns.
	sed 's/\r$$//; s/$$/\r/' $(CWSDPMI)/cwsdpmi.doc > $(DOSDIST)/CWSDPMI.DOC
	sed 's/\r$$//; s/$$/\r/' dos/DISTREAD.TXT > $(DOSDIST)/README.TXT
	rm -f secretorb-dos32.zip secretorb-720k.img
	cd dist && zip -qr ../secretorb-dos32.zip SECORB
	mkfs.msdos -F 12 -n SECRETORB -C secretorb-720k.img 720 > /dev/null
	mcopy -i secretorb-720k.img $(DOSDIST)/* ::/
	@echo "=== 720KB floppy image ==="
	@mdir -i secretorb-720k.img ::

dos-clean:
	rm -rf $(DOSDIR) dist secretorb-dos32.zip secretorb-720k.img

# Windows 32-bit
win32: $(BINDIR)
	$(FPC) $(FPCFLAGS) $(WIN32_FLAGS) -o$(BINDIR)/secretorb.exe secretorb.pas
	$(FPC) $(FPCFLAGS) $(WIN32_FLAGS) -o$(BINDIR)/editor.exe editor.pas

# Create release package (fits on 720KB floppy)
release: native
	@echo "Creating release package..."
	mkdir -p release
	cp $(BINDIR)/secretorb release/
	cp $(BINDIR)/editor release/
	cp $(DATADIR)/world.dat release/
	@echo "Package size:"
	@du -sh release/
	@echo "Individual files:"
	@ls -lh release/

clean: dos-clean
	rm -rf $(BINDIR) release
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.ppu
	rm -f *.o *.ppu

# Size check for 720KB floppy target
sizecheck: release
	@TOTAL=$$(du -s release | cut -f1); \
	if [ $$TOTAL -gt 720 ]; then \
		echo "WARNING: Release exceeds 720KB ($$TOTAL KB)"; \
	else \
		echo "OK: Release fits in 720KB ($$TOTAL KB used)"; \
	fi
