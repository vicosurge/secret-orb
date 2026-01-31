# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Secret Orb is a text-based adventure game written in Free Pascal, designed to fit on a 720KB floppy disk. The project consists of two main programs:
- **secretorb**: The game runtime that loads and plays world files
- **editor**: A TUI-based world editor for creating and modifying game worlds

## Build Commands

### Building Locally

Build for current platform (Linux/macOS):
```bash
cd pascal
make native
# Or: ./build.sh native
```

Build for specific platforms:
```bash
cd pascal
make dos32      # DOS 32-bit DPMI (requires go32v2 cross-compiler)
make win32      # Windows 32-bit
```

Build and check size constraints:
```bash
cd pascal
make release
make sizecheck  # Verifies fit within 720KB floppy limit
```

Clean build artifacts:
```bash
cd pascal
make clean
```

### Running

```bash
cd pascal/bin
./secretorb [world.dat]
./editor
```

### CI/CD

GitHub Actions builds for Linux, Windows, and DOS on every push to main. See `.github/workflows/pascal.yml`.

## Architecture

### Module Structure

The codebase is organized into modular units in `pascal/src/`:

- **gamedata.pas**: Core data structures and constants
  - Defines `TRoom`, `TGameObject`, `TMob`, and `TGameWorld` records
  - `TDirection` enum: 6 directions (North, South, East, West, Up, Down)
  - Constants: `MAX_ROOMS=256`, `MAX_OBJECTS=128`, `MAX_MOBS=64`, `MAX_INVENTORY=8`
  - Helper functions for finding objects/mobs by ID or name, parsing directions

- **datafile.pas**: World file I/O (dual format support)
  - Supports binary format (default, space-efficient) and text format (legacy)
  - Binary: Packed records with 'SORB' magic signature, BlockRead/BlockWrite
  - Text: INI-style format with sections `[WORLD]`, `[ROOM:n]`, `[OBJECT:n]`, `[MOB:n]`
  - Auto-detects format on load, saves binary by default
  - Includes format converter functions and flag serialization

- **display.pas**: Text display abstraction layer
  - Wraps CRT unit for cross-platform terminal operations
  - Functions: `ClearScreen`, `WriteAt`, `WriteCenter`, `WriteWrapped`, `ReadLine`
  - Drawing primitives: `DrawBox`, `DrawHLine`
  - 80x25 character screen assumed

- **gamecore.pas**: Game engine and command processing
  - Command parser: converts player input to `TCommandType` enum
  - Command handlers: movement, examine, take, drop, use, open, read, talk, inventory
  - Game loop: `RunGame` function drives the main gameplay

### Program Entry Points

- **secretorb.pas**: Game launcher
  - Shows title screen
  - Loads world file (default: `world.dat` or command-line argument)
  - Hands off to `RunGame` in gamecore.pas

- **editor.pas**: World editor TUI
  - State machine with `TEditorState` enum
  - Menu-driven interface for editing rooms, objects, mobs
  - Load/save world files
  - Full CRUD operations on game entities

### Data File Format

World files support two formats with automatic detection:

#### Binary Format (Default)

The editor saves in binary format by default for space efficiency. Structure:

- **Header (16 bytes)**: Magic signature 'SORB', version, counts, start room
- **Rooms**: Packed TRoomBin records (313 bytes each)
  - Supports 6 directions: North, South, East, West, Up, Down
- **Objects**: Packed TGameObjectBin records (242 bytes)
- **Mobs**: Packed TMobBin records (339 bytes)

Format validation: Magic signature check, version verification, IOResult error handling.

#### Text Format (Legacy/Manual Editing)

Text-based INI-style format, still fully supported for loading:

```ini
[WORLD]
TITLE=Game Title
START=1

[ROOM:id]
NAME=Room Name
DESC=Description
NORTH=room_id
SOUTH=room_id
EAST=room_id
WEST=room_id
UP=room_id
DOWN=room_id

[OBJECT:id]
NAME=Object Name
DESC=Description
ROOM=room_id
CARRIEDBY=mob_id
FLAGS=pickup,use,open,read
USETEXT=Text shown when used

[MOB:id]
NAME=Mob Name
DESC=Description
ROOM=room_id
DIALOGUE=What the mob says
```

#### Format Conversion

Use the converter tool to migrate text worlds to binary:

```bash
make converter
bin/converter input.txt output.dat
```

The game auto-detects format on load (checks for 'SORB' magic signature).

## Compiler Flags

Free Pascal compiler flags (see Makefile):
- `-O2`: Optimization level 2
- `-XX`: Smart linking (removes unused code)
- `-CX`: Create smartlinkable units
- `-Xs`: Strip symbols from executable
- `-Fu<dir>`: Search directory for units

Target-specific:
- `-Tgo32v2`: DOS 32-bit DPMI (requires CWSDPMI at runtime)
- `-Twin32`: Windows 32-bit
- `-Twin16 -WD`: DOS 16-bit real mode (less common)

## Size Constraints

The project must fit on a 720KB (737,280 bytes) floppy disk. CI checks verify this constraint:
- secretorb + editor + world.dat < 720KB
- Use size-optimized compiler flags (`-XX`, `-CX`, `-Xs`)
- Minimize world file content in default distribution

## Development Workflow

When modifying game logic:
1. Edit the appropriate unit file in `pascal/src/`
2. Rebuild with `make native`
3. Test by running `bin/secretorb` with `bin/world.dat`

When modifying world content:
1. Run `bin/editor` to edit `data/world.dat`, or
2. Edit `data/world.dat` directly in a text editor
3. Copy to `bin/` for testing

When adding new entities (rooms, objects, mobs):
- Respect maximum limits defined in gamedata.pas constants
- Use sequential IDs starting from 1
- Set `Active=True` (editor handles this automatically)
