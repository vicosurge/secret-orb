# Migration Guide: Text to Binary Format

This guide explains how to migrate your existing Secret Orb world files from the old text format to the new binary format with 6-direction movement support.

## What's New

### 6-Direction Movement

The game now supports vertical movement in addition to the original 4 cardinal directions:

- **North, South, East, West** - Original horizontal movement
- **Up, Down** - New vertical movement (stairs, ladders, towers, pits, etc.)

### Binary File Format

World files now use a compact binary format by default:

- **30-50% smaller** file sizes
- Faster load/save operations
- Still under 720KB floppy disk constraint
- Automatic format detection on load

### Backward Compatibility

- **Text format still works** - The game auto-detects and loads text files
- **No breaking changes** - Existing worlds load without modification
- **Gradual migration** - Convert worlds at your own pace

## Migration Steps

### Quick Migration

For most users, migration is a simple 3-step process:

1. **Build the converter tool:**
   ```bash
   cd pascal
   make converter
   ```

2. **Convert your world file:**
   ```bash
   bin/converter data/world.dat data/world.bin
   ```

3. **Test the converted file:**
   ```bash
   bin/secretorb data/world.bin
   ```

4. **Replace the original (optional):**
   ```bash
   # Backup first!
   cp data/world.dat data/world.dat.backup
   mv data/world.bin data/world.dat
   ```

### Adding Vertical Movement

If you want to add vertical exits to your existing world:

1. **Open your world in text format:**
   - Either keep it in text format, or
   - Convert back from binary for editing (see below)

2. **Add UP and DOWN exits to rooms:**
   ```ini
   [ROOM:1]
   NAME=Ground Floor
   DESC=A large hall with a staircase leading upward.
   NORTH=2
   SOUTH=0
   EAST=0
   WEST=0
   UP=10        ; Add stairs going up to room 10
   DOWN=0       ; No exit down
   ```

3. **Create the target rooms:**
   ```ini
   [ROOM:10]
   NAME=Second Floor
   DESC=An upper level overlooking the hall below.
   NORTH=0
   SOUTH=0
   EAST=0
   WEST=0
   UP=0
   DOWN=1       ; Stairs back down to room 1
   ```

4. **Save and convert:**
   ```bash
   bin/converter data/world.dat data/world.bin
   ```

### Using the Editor

The editor automatically saves in binary format:

1. **Open your existing world:**
   ```bash
   bin/editor
   # Choose "Load World", select your .dat file
   ```

2. **Edit rooms to add vertical exits:**
   - Select "List Rooms"
   - Choose a room to edit
   - Tab to the "Up Exit" and "Down Exit" fields
   - Enter room IDs (or 0 for no exit)

3. **Save:**
   - Choose "Save World"
   - File is automatically saved in binary format

## Format Details

### Text Format (Legacy)

```ini
[ROOM:1]
NAME=Room Name
DESC=Description
NORTH=2
SOUTH=0
EAST=3
WEST=0
UP=10        ; New in v0.2+
DOWN=0       ; New in v0.2+
```

- 0 means "no exit"
- Room IDs must match existing room IDs
- UP/DOWN fields are optional (default to 0)

### Binary Format

- Magic signature: `SORB`
- Version: 1
- Fixed-length packed records
- All 6 directions stored for each room
- Automatic validation on load

### Format Detection

The game automatically detects the format:

```pascal
if file starts with 'SORB' then
  Load as binary
else
  Load as text
```

No manual format specification needed!

## Converter Tool Usage

### Basic Syntax

```bash
converter <input-file> <output-file>
```

### Examples

**Convert text to binary:**
```bash
bin/converter myworld.txt myworld.dat
```

**Batch convert multiple worlds:**
```bash
for f in worlds/*.txt; do
  bin/converter "$f" "${f%.txt}.dat"
done
```

**Convert with backup:**
```bash
cp world.dat world.dat.backup
bin/converter world.dat world.bin
mv world.bin world.dat
```

## Troubleshooting

### "Error: Failed to load input file"

**Cause:** Input file doesn't exist or has invalid format

**Solution:**
- Check file path is correct
- Verify file is readable
- Ensure file is valid Secret Orb format (text or binary)

### "Error: Failed to save output file"

**Cause:** Output directory doesn't exist or is read-only

**Solution:**
- Create output directory: `mkdir -p output/`
- Check write permissions: `ls -ld output/`

### Binary file loads but rooms seem broken

**Cause:** Corrupted binary file or version mismatch

**Solution:**
- Reconvert from text source
- Check converter completed without errors
- Verify file size is reasonable (not 0 bytes)

### Editor doesn't show Up/Down exits

**Cause:** Using old editor version

**Solution:**
- Rebuild editor: `make clean && make native`
- Verify you're running the new binary

## Best Practices

### Keep Text Sources

**Recommended workflow:**

1. Maintain worlds in **text format** for version control
2. Store in `worlds-src/*.txt`
3. Convert to binary for distribution: `worlds/*.dat`
4. Commit only text files to git

**Example project structure:**
```
secret-orb/
├── pascal/
│   └── data/
│       └── world.dat          # Binary (gitignored)
└── worlds-src/
    ├── myworld.txt            # Text source (committed)
    └── dungeon.txt            # Text source (committed)
```

### Testing Migrations

Before migrating production worlds:

1. **Test on a copy:**
   ```bash
   cp world.dat world-test.dat
   bin/converter world-test.dat world-test.bin
   bin/secretorb world-test.bin
   ```

2. **Verify all content loads:**
   - Walk through all rooms
   - Check objects are present
   - Test NPC dialogue
   - Verify inventory system

3. **Compare file sizes:**
   ```bash
   ls -lh world.dat world.bin
   # Binary should be 30-50% smaller
   ```

### Gradual Migration

You don't have to migrate everything at once:

- Keep critical worlds in text format initially
- Migrate world-by-world as you test them
- Both formats work side-by-side

## Rollback

If you need to go back to text format:

### Option 1: Restore from backup

```bash
cp world.dat.backup world.dat
```

### Option 2: Manual recreation

The text format is simple enough to recreate manually if needed. See [README.md](README.md) for format specification.

## FAQ

**Q: Do I have to migrate?**

A: No. Text format still works perfectly. Migrate only if you want smaller files or plan to use the editor.

**Q: Can I edit binary files manually?**

A: No. Use the editor or convert to text, edit, then convert back.

**Q: Will my old saved games work?**

A: Yes. The game auto-detects format on load. Old and new worlds work interchangeably.

**Q: What about mods/custom worlds from others?**

A: Both formats work. The game loads whatever format the file is in.

**Q: Can I convert binary back to text?**

A: Currently not directly supported. Use the editor to view/modify binary worlds. Exporting to text format may be added in a future version.

**Q: How do I know which format a file is?**

Check the magic bytes:
```bash
head -c 4 world.dat
# If it prints "SORB", it's binary
# Otherwise, it's text
```

## Support

If you encounter issues during migration:

1. Check this guide's Troubleshooting section
2. See [CLAUDE.md](CLAUDE.md) for technical details
3. Open an issue on GitHub with:
   - Error messages
   - File sizes (before/after)
   - Steps to reproduce

---

*Happy adventuring with vertical movement!*
