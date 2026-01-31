# Secret Orb

```
  ___                  _      ___       _
 / __| ___  __ _ _ ___| |_   / _ \ _ _ | |__
 \__ \/ -_)/ _| '_/ -_)  _| | (_) | '_|| '_ \
 |___/\___|\__|_| \___|\__|  \___/|_|  |_.__/

        A Text Adventure for the Ages
```

A retro text-based adventure game designed to fit on a single 720KB floppy disk. Written in Free Pascal for maximum portability across DOS, Windows, and Unix-like systems.

---

## 🎮 About

**Secret Orb** is a modular, lightweight text adventure game that brings back the classic feel of early computer gaming. Explore mysterious rooms, collect objects, interact with characters, and uncover secrets—all through a simple text interface.

### Features

- 🎯 **Compact Design**: Entire game fits on a 720KB floppy disk
- 🖥️ **Cross-Platform**: Runs on DOS (16-bit and 32-bit), Windows, Linux, and other Unix systems
- 🏗️ **Modular Architecture**: Clean separation between game engine and world data
- ✏️ **World Editor**: Included TUI-based editor for creating your own adventures
- 📄 **Dual Format Support**: Binary format for space efficiency, text format for easy editing
- 🧭 **6-Direction Movement**: Navigate in all directions including up and down
- 🎨 **Retro Aesthetics**: 80x25 character display with classic CRT colors

### Project Goals

- Target retro hardware (x86 compatible, 720KB floppy capacity)
- Maximize portability through Free Pascal
- Keep codebase simple and educational
- Enable easy world creation through text-based data files

---

## 📦 Download & Installation

### Pre-built Binaries

Download the latest release from [GitHub Actions artifacts](../../actions):

- **Linux**: `secretorb-linux-x64.tar.gz`
- **Windows**: `secretorb-win32.zip`
- **DOS**: `secretorb-dos32.zip` (requires CWSDPMI.EXE for DPMI support)

### Linux/Unix

```bash
# Extract archive
tar -xzf secretorb-linux-x64.tar.gz
cd secretorb-linux

# Run the game
./secretorb

# Or run the editor
./editor
```

### Windows

```cmd
REM Extract secretorb-win32.zip to a folder

REM Run the game
secretorb.exe

REM Or run the editor
editor.exe
```

### DOS

```
C:\> SECORB.EXE
```

*Note: DOS 32-bit version requires CWSDPMI.EXE (freely available DPMI host)*

---

## 🎲 How to Play

### Starting the Game

Run `secretorb` (or `secretorb.exe`) to start. The game will load `world.dat` by default, or you can specify a different world file:

```bash
./secretorb myworld.dat
```

### Commands

Secret Orb uses a simple text parser. Type commands at the prompt:

#### Movement
- `north` or `n` - Move north
- `south` or `s` - Move south
- `east` or `e` - Move east
- `west` or `w` - Move west
- `up` or `u` - Go up (stairs, ladders, etc.)
- `down` or `d` - Go down (descend, climb down, etc.)

#### Interaction
- `look` - Examine your surroundings
- `examine <object>` or `look at <object>` - Inspect an object or character
- `take <object>` - Pick up an object
- `drop <object>` - Drop an object from inventory
- `use <object>` - Use an object
- `open <object>` - Open something
- `read <object>` - Read text on an object
- `talk <character>` - Speak with a character

#### Inventory & Help
- `inventory` or `i` - Check your inventory
- `help` - Show help screen
- `quit` or `q` - Exit the game

### Example Session

```
> look
You stand in a grand entrance hall. Stone walls rise high around you,
covered in faded tapestries depicting ancient battles. A cold draft
blows from the north.

Exits: North
You see: Rusty Key

> take key
You pick up the Rusty Key.

> north
You move north.

> examine scroll
A yellowed parchment scroll covered in faded runes and mystical symbols.

> read scroll
The scroll reads: "Seek the orb in the darkest room, where shadows
dance and secrets loom."
```

---

## 🛠️ Development

### Building from Source

#### Prerequisites

- Free Pascal Compiler (FPC) 3.2.0 or later
- Make (optional, for Makefile builds)

#### Build Commands

```bash
# Clone the repository
git clone https://github.com/yourusername/secret-orb.git
cd secret-orb/pascal

# Build for your platform
make native

# Or use the build script
./build.sh native

# Build the converter tool
make converter

# Cross-compile for other platforms
make dos32    # DOS 32-bit DPMI
make win32    # Windows 32-bit

# Check size constraints (720KB floppy)
make sizecheck

# Clean build artifacts
make clean
```

#### Compiler Flags

The build uses these optimization flags:
- `-O2` - Optimization level 2
- `-XX` - Smart linking (dead code elimination)
- `-CX` - Create smartlinkable units
- `-Xs` - Strip symbols from executable

### Project Structure

```
pascal/
├── secretorb.pas      # Game launcher (main entry point)
├── editor.pas         # World editor (main entry point)
├── src/
│   ├── gamedata.pas   # Core data structures (rooms, objects, mobs)
│   ├── datafile.pas   # World file I/O (text and binary formats)
│   ├── display.pas    # Terminal display abstraction
│   └── gamecore.pas   # Game engine & command processing
├── tools/
│   └── converter.pas  # Text-to-binary format converter
├── data/
│   └── world.dat      # Default game world
└── bin/               # Compiled executables (created by build)
```

### Creating Custom Worlds

#### Using the Editor

Run the world editor to create or modify game worlds:

```bash
./editor
```

The editor provides a menu-driven interface to:
- Add/edit/delete rooms with descriptions and exits
- Create objects with properties (pickup, use, open, read)
- Add NPCs (mobs) with dialogue
- Save worlds to `.dat` files

#### Manual Editing

World files support two formats:

1. **Binary Format** (default): Compact, space-efficient format saved automatically by the editor
2. **Text Format**: Human-readable INI-style format for manual editing

You can edit text format files directly and convert them to binary using the converter tool:

```bash
./converter world.txt world.dat
```

**Text Format Example:**

```ini
[WORLD]
TITLE=My Adventure
START=1

[ROOM:1]
NAME=Starting Room
DESC=You are in a small room.
NORTH=2
SOUTH=0
EAST=0
WEST=0
UP=3
DOWN=0

[OBJECT:1]
NAME=Magic Sword
DESC=A gleaming sword with mystical runes.
ROOM=1
CARRIEDBY=0
FLAGS=pickup,use
USETEXT=The sword glows brightly!

[MOB:1]
NAME=Mysterious Wizard
DESC=An old wizard with a long grey beard.
ROOM=2
DIALOGUE=Greetings, traveler! Seek ye the Secret Orb?
```

See [CLAUDE.md](CLAUDE.md) for detailed format specification.

### Contributing

Contributions are welcome! The codebase is intentionally simple and educational.

**Guidelines:**
- Keep the 720KB size constraint in mind
- Follow the existing Pascal coding style
- Test on multiple platforms when possible
- Update documentation for new features

---

## 🎯 Technical Details

### Size Constraints

The entire game (executables + data) must fit on a 720KB floppy disk (737,280 bytes). This constraint drives many design decisions:

- Aggressive compiler optimizations
- Efficient data structures
- Text-based data format
- No embedded assets or multimedia

### Limits

- Maximum 256 rooms
- Maximum 128 objects
- Maximum 64 mobs (NPCs)
- Maximum 8 inventory slots
- 40 character room names
- 255 character descriptions

### Platform Compatibility

**Tested on:**
- Linux x64 (Ubuntu 22.04+)
- Windows 10/11 (32-bit executable)
- DOS (32-bit DPMI via go32v2)

**Should work on:**
- Any x86 system with FPC support
- FreeBSD, OpenBSD
- DOS with DPMI host (CWSDPMI, DOS4GW, etc.)
- Windows XP through Windows 11

---

## 📜 License

[Add your license here - MIT, GPL, etc.]

---

## 🤝 Acknowledgments

Built with [Free Pascal](https://www.freepascal.org/) - a mature, stable, and portable Pascal compiler.

Inspired by classic text adventures from the golden age of computing.

---

## 🔗 Links

- [GitHub Repository](https://github.com/vicosurge/secret-orb)
- [Issue Tracker](https://github.com/vicosurge/secret-orb/issues)
- [Free Pascal](https://www.freepascal.org/)

---

*Seek the Secret Orb. Adventure awaits.*
