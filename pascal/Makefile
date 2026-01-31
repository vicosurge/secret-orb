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
DOS32_FLAGS = -Tgo32v2
WIN32_FLAGS = -Twin32

.PHONY: all clean native dos dos32 win32 release

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

# DOS 16-bit (requires DOS target support in FPC)
dos: $(BINDIR)
	$(FPC) $(FPCFLAGS) $(DOS_FLAGS) -o$(BINDIR)/SECORB.EXE secretorb.pas
	$(FPC) $(FPCFLAGS) $(DOS_FLAGS) -o$(BINDIR)/EDITOR.EXE editor.pas

# DOS 32-bit DPMI (GO32V2)
dos32: $(BINDIR)
	$(FPC) $(FPCFLAGS) $(DOS32_FLAGS) -o$(BINDIR)/secretorb.exe secretorb.pas
	$(FPC) $(FPCFLAGS) $(DOS32_FLAGS) -o$(BINDIR)/editor.exe editor.pas

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

clean:
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
