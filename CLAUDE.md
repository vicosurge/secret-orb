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

### Building for DOS

```bash
cd pascal
dos/bootstrap-toolchain.sh   # once: builds the go32v2 cross-compiler (~1 min)
make dos32                   # bin/dos/*.EXE
make dos-test                # runs them on FreeDOS under QEMU
make dos-dist                # secretorb-dos32.zip + secretorb-720k.img
```

No distribution packages a go32v2 cross-compiler, so `dos/bootstrap-toolchain.sh`
builds one from pinned Free Pascal sources into `~/.cache/secretorb-dos`. Two
things about that build are load-bearing and easy to trip over:

- The go32v2 RTL needs **djgpp binutils** to assemble `v2prt0.as` into a COFF-go32
  object; host binutils cannot. The script pins a djgpp release for this and uses
  nothing from it but `as`/`ld`/`ar`/`strip`.
- **`Crt` is not in the RTL** for go32v2 — it lives in the `rtl-console` package,
  and cross-building the package set dies inside `fpmake` with a heap overflow.
  The script compiles that one unit directly and skips the packages stage. It is
  not optional: `secorb.pas`, `editor.pas`, `gamecore.pas` and `display.pas` all
  use it.

`make dos32` builds **`secorb.pas`**, not `secretorb.pas` — the 8.3-named duplicate
is what ships on DOS, which is the other reason the two files must stay identical.
It also builds `VALIDATE.EXE`, `CONVERT.EXE`, `PAIRTEST.EXE` and `EVENTTST.EXE`,
which are not part of the distribution: they are what `make dos-test` runs inside
FreeDOS, comparing each one's output against the native run. Those four are plain
`WriteLn` programs, so DOS output redirection captures them; the game and the
editor use `Crt`, which writes to video memory and reads the BIOS keyboard, and
cannot be driven through a pipe at all. `EVENTTST.EXE` is the only way any of the
*engine* is exercised under DOS — `events.pas` uses `GameData` and nothing else,
which is what makes it drivable from a console program. Its scratch files carry
8.3 names for the same reason: the volume it runs on is FAT, and the kernel need
not support long ones.

`make dos-dist` builds a real 720KB FAT12 floppy image alongside the zip. That is
the size constraint as a build step rather than a warning: if the distribution
outgrows the disk, `mcopy` fails and the build fails with it.

`pascal/BUILD.BAT` is unchanged and still builds Secret Orb from inside DOS with a
DOS-hosted FPC. See `pascal/dos/README.md` for the details of both paths.

### Running

```bash
cd pascal/bin
./secretorb [world.dat]
./editor
```

### CI/CD

GitHub Actions builds for Linux, Windows, and DOS on every push to main. See
`.github/workflows/pascal.yml`. The DOS job builds the cross-compiler (cached on
`pascal/dos/versions.sh`), then boots the binaries on FreeDOS under QEMU: the unit
tests must pass under DOS, the shipped world must validate, and the DOS converter
must produce byte-identical world files to the Linux one.

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
  - `WriteWrappedMax` is the bounded form: it stops after N rows and returns how
    many it used, so a caller laying out a screen knows where the text ended.
    `WriteWrapped` is a thin call to it
  - Drawing primitives: `DrawBox`, `DrawHLine`
  - 80x25 character screen assumed

- **worldval.pas**: Editor-side world checks — **never used by the game**
  - `ValidateWorld` reports broken exits, unreachable rooms, missing entity
    references, win conditions that cannot be met, one-way exits, and paragraph
    numbers naming empty slots. The rules mirror `validate()` in
    `web/editor.html`, so all three editors agree about what is wrong with a
    world; the two implementations are checked against each other by feeding
    the same broken world to both
  - `PairExits` counts, and optionally fills, the exits whose opposite side is
    free. It never overwrites a return exit that already leads somewhere else —
    that is either a deliberate one-way link or a mistake the validator reports.
    Both Pascal editors and the browser editor's *Link back* button behave the
    same way
  - `CheckEvents` is the largest of these checks, because events are where the
    mistake this unit exists for is most available: everything an event names
    is a number, and a number naming nothing is invisible at run time —
    `FireEvents` skips a trigger that never matches and `RunActions` skips an
    action whose target is not there, both without a word. Its rules follow
    `events.pas` exactly and have to be read with it. Three of the
    interpreter's conventions drive them:
    - a `TriggerID` of 0 means **any**, so an author can write one event for
      every object; it is not a missing reference
    - a `TriggerID2` set on a trigger whose hook passes 0 can never match, so
      the event is *dead* — an error, not a warning
    - `FireEvents` stops at `EventCount`, so an active event above it is dead
      too
  - `WriteParaXRef` writes the author's cross-reference (see below)
  - `secretorb.pas` must not list this unit in `uses`. The game runs from a
    720KB floppy and has no business carrying authoring checks

- **events.pas**: The event interpreter — triggers, conditions, actions
  - Engine code: it ships in the game, unlike `worldval.pas`. Execution is the
    engine's job; telling an author their event is broken is the editor's
  - Uses `GameData` and **nothing else**. It must never use `GameCore` —
    `TGame` lives there, so a call in that direction would be a unit cycle —
    and it never touches `Crt` or `Display`. Everything the player should see
    comes back in a `TEventOutcome` that `gamecore.pas` drains
  - That constraint is what makes it testable: `tools/eventtest.pas` drives it
    from a plain `WriteLn` program, which CI runs on Linux *and* under
    FreeDOS. `gamecore.pas` cannot be tested that way at all
  - `FireEvents` fires **every** matching event in ascending slot order, not
    just the first. That is how an author writes more than `MAX_ACTIONS`
    actions for one trigger: two events sharing a trigger
  - A cascade (`atSetFlag` → `etFlagSet` → `atSetFlag` …) is bounded four
    ways: flag writes are **edge-triggered**, so re-setting a set flag fires
    nothing; `Fired` is set *before* the actions run, so a one-shot cannot
    re-enter itself; `MAX_EVENT_DEPTH` caps recursion; and `MAX_TURN_ACTIONS`
    caps the whole batch, which bounds a wide fan-out too
  - `atTeleportPlayer` is **reported, not applied** — the room change has to
    run through `EnterRoom` in gamecore, which knows about first-visit scoring
    and paragraphs

- **gamecore.pas**: Game engine and command processing
  - Command parser: converts player input to `TCommandType` enum. Only the verb is
    upper-cased; the noun keeps its typed case so `SAVE`/`LOAD` file names survive
    on case-sensitive filesystems
  - Command handlers: movement, examine, take, drop, use, open, read, talk,
    give, inventory, save, load, score, exits, again
  - **Two verbs take two nouns**, and only those two: `USE X ON Y` (also
    `WITH`, also `TO`) and `GIVE X TO Y` (also `ON`). `ParseCommand` splits
    the noun on the preposition through `SplitPrep` and returns the second
    half in `TGame.LastNoun2`; every other verb keeps its noun whole, which
    is what stops `TALK TO WIZARD` losing its noun, `DROP TORCH ON FLOOR`
    becoming two objects, and `SAVE game to keep.dat` losing its file name.
    These are the only two commands that can raise `etUseObjectOn` and
    `etGiveTo`
  - `etUseObjectOn`'s second target is **always an object** — handing
    something to a person is `GIVE`, which has its own trigger — so an
    event's `TriggerID2` names an object and the validator checks it as one.
    `USE` on a mob answers *"Try giving it to them instead."*
  - **`GIVE` moves nothing by itself.** What a gift means is the author's
    decision, so the transfer belongs in the event's actions; without an
    event the object stays in hand and the mob refuses it. An item silently
    swallowed by an NPC nobody wrote a response for could strand the game
  - `EXITS` reports the way out through `LastMessage`; `ExitsLine` is the single
    place exits are turned into prose, called by both it and `ShowRoom`
  - `AGAIN`/`G` replays `TGame.PrevCmd`. Only turn-consuming commands are
    recorded there, so `AGAIN` can never repeat itself, a save, or a help
    screen. It is resolved before dispatch, so the replayed command behaves in
    every respect — turn count included — as if the player had retyped it.
    `PrevNoun2` is saved beside `PrevNoun`, or `AGAIN` after `USE KEY ON DOOR`
    would replay the bare `USE KEY` — a different command firing a different
    trigger
  - `LastMessage` is wrapped through `WriteWrappedMax` and bounded to the rows
    left above the prompt. Writing it unwrapped let the terminal wrap it, which
    pushed the prompt off the row `PromptY` recorded and drew `>` on top of the
    message
  - Game loop: `RunGame` function drives the main gameplay
  - `ShowRoom` records the row it drew the `>` prompt on in `TGame.PromptY`, which
    `RunGame` uses to position input
  - Scoring and endings: rooms and objects carry `Points`, awarded once on first
    visit / first take (tracked by `World.Visited` and `World.Taken`). The game is
    won by reaching `WinRoomID` while carrying `WinObjectID`; either may be 0 to
    disable that half of the condition. Meta commands (help, score, save, load,
    quit) do not consume a turn
  - Story paragraphs: long-form scene text that fires at a moment rather than on
    demand. Every paragraph reaches the player through one private helper,
    `ShowParagraph`, so the per-world booklet flag is honoured in exactly one
    place. The six trigger sites are:

    | Trigger | Where |
    |---------|-------|
    | `World.IntroPara` | `RunGame`, before the loop |
    | `Room.FirstVisitPara` | `MovePlayer`, where `Visited` is set — **and** the start-room block in `RunGame`, which bypasses `MovePlayer` |
    | `Object.FirstTakePara` | `HandleTake`, where `Taken` is set |
    | `Mob.FirstTalkPara` | `HandleTalk`, guarded by the new `World.Talked` bitmap |
    | `World.WinPara` | `ShowEnding`, before the score summary |
    | `World.LosePara` | `RunGame`, when the loop ends without `gsWon` — quitting is its own ending |

    Since version 4 there is a seventh route: an event's `atShowParagraph`
    action. It reaches the player through `ApplyOutcome` in gamecore.pas, which
    calls the same `ShowParagraph`, so booklet mode still needs no special
    case. `worldval.pas`'s `DescribeTriggers` has to know about it too, or an
    event-fired paragraph gets cross-referenced as "fired by: NOTHING".

    Because the three trigger bitmaps are the same ones scoring already used and
    the save format already stored, a restored game never replays a scene.

### Story Paragraphs and the Booklet

A world carries up to `MAX_PARAGRAPHS` (128) numbered paragraphs of up to
`MAX_PARA_LEN` (1600) characters. **A paragraph's number is its array index and is
never reused or shifted** — deleting paragraph 7 blanks slot 7 rather than compacting,
because those numbers get printed in a booklet and must not move under the player.
`SetParagraph` in gamedata.pas enforces this; `ParaCount` tracks the highest used slot.

`TParagraphArray` is explicitly `AnsiString` (`TParaText`), not `string`. These units
compile with short strings on, where a plain `string` is a 256-byte `ShortString` —
too short for a paragraph, and an array of 128 of them would be 32KB of static data
instead of a table of pointers. Nothing ever `BlockWrite`s or `FillChar`s
`TGameWorld` wholesale, which is what makes refcounted fields in that record safe.

Setting `WF_BOOKLET` in `World.WorldFlags` switches the engine from printing a
paragraph to citing it (`Read paragraph 12 in your booklet.`) — the Wasteland
copy-protection move. It is off by default. All three editors can export a printable
booklet whose numbering matches what the game cites.

Display support lives in display.pas: `WrapText` is the single word-wrap
implementation (`WriteWrapped` is a thin caller), and `ShowTextPage` pages anything
longer than a screen with a `-- More --` prompt.

### Program Entry Points

- **secretorb.pas**: Game launcher
  - Loads the world *first*, then shows the title screen, so the title screen can
    name the world. **`secorb.pas` is a byte-identical DOS 8.3 duplicate driven by
    `BUILD.BAT` — any change here must be mirrored there or the FreeDOS build
    silently diverges.**
  - World file defaults to `world.dat`, or a command-line argument
  - Hands off to `RunGame` in gamecore.pas

- **editor.pas**: World editor TUI
  - State machine with `TEditorState` enum
  - Menu-driven interface for editing rooms, objects, mobs
  - Load/save world files
  - Full CRUD operations on game entities

### Data File Format

World files support two formats with automatic detection:

#### Binary Format (Default)

The editor saves in binary format by default for space efficiency. Current version
is **4**; versions 1, 2 and 3 still load, with the new fields defaulting to 0 and (for
version 1) the title derived from the file name. Saves always write version 4.

- **Header**: 69 bytes — magic signature 'SORB', version, counts, start room, title,
  win room ID, win object ID, max score, then the story fields
- **Rooms**: Packed TRoomBinV3 records — 317 bytes
- **Objects**: Packed TGameObjectBinV3 records — 246 bytes
- **Mobs**: Packed TMobBinV3 records — 343 bytes
- **Paragraphs**: a trailing variable-length section

Version 3 keeps the header at exactly 69 bytes: the eight bytes that were `Reserved`
in version 2 are now named fields, and version 2 writers already zero-filled them.

| Offset | Field |
|--------|-------|
| 61 | `IntroPara: Word` |
| 63 | `WinPara: Word` |
| 65 | `LosePara: Word` |
| 67 | `WorldFlags: Byte` (bit 0 = `WF_BOOKLET`) |
| 68 | `Reserved: Byte` |

Each version 3 entity record is its version 2 record with **one Word appended**, so
the layouts stay trivially derivable: `FirstVisitPara` at room +315, `FirstTakePara`
at object +244, `FirstTalkPara` at mob +341. The paragraph section follows the mob
records and is self-describing:

```
Word  ParaCount              { highest used number, not a count of non-empty }
repeat ParaCount times:
  Word         Length        { 0 = unused slot, so numbering survives a deletion }
  Byte[Length] text          { Latin-1, #13#10 for hard line breaks }
```

#### Version 4: events, flags and counters

Version 4 is version 3 with three self-describing sections appended after the
paragraph blob. No room, object or mob record changes, so `ReadBinaryV2Or3`
absorbs v4 with one `if Version >= 4` and the dispatch reads `2, 3, 4:`.

```
Word  nEvents                   { records that follow - NOT the highest slot }
repeat nEvents times:
  Word  Size                    { bytes after this field }
  Word  Number                  { the slot, 1..MAX_EVENTS }
  Byte  NameLen ; Byte[NameLen] Name
  Byte  TriggerType             { Ord(TEventTrigger) }
  Word  TriggerID ; Word TriggerID2
  Byte  Flags                   { bit 0 OneShot, bit 1 Enabled }
  Byte  CondCount
  repeat CondCount times:       { 6 bytes each, only the used ones }
    Byte CondType ; Word TargetID ; SmallInt Value ; Byte Negate
  Byte  ActionCount
  repeat ActionCount times:     { only the used ones }
    Byte ActionType ; Word TargetID ; SmallInt Value
    Byte TextLen ; Byte[TextLen] Text
Word  FlagNameCount    ; repeat: Byte Len + Len bytes
Word  CounterNameCount ; repeat: Byte Len + Len bytes
```

**The event record is the one variable-length record in the format**, and
deliberately so: a fixed record would reserve `MAX_ACTIONS` × `MAX_EVENT_TEXT`
per event — 764 bytes where a realistic event uses under a hundred. The
leading `Size` lets a reader skip a record whose tail it does not understand
and makes truncation detectable. A world with no events costs 6 bytes.

**An event's slot number is its identity and slots are never shifted**, the
same rule paragraphs follow — but for a stronger reason than booklet
numbering. A save game's `Fired` and `EvEnabled` bitmaps are indexed by slot,
and `atEnableEvent`/`atDisableEvent` name a slot, so compacting on save would
silently repoint every existing save at the wrong events. Because each record
carries its own number, a gap costs *nothing* here, where a deleted paragraph
still costs its two-byte zero length. `TWorldEvent` therefore has no `ID`
field.

Two different numbers are easy to confuse here. **In memory**,
`TGameWorld.EventCount` is the highest used slot — `FireEvents` walks
`1..EventCount` and stops, so an active event above it never fires, which is
why the validator checks for one. **On disk**, the leading `Word` is the
number of records that follow, gaps skipped, because each record names its own
slot. The two are equal only in a world with no gaps.

Enum fields go on disk as explicit `Byte` ordinals, never as the enum types: an
FPC enum is four bytes by default and its width is a compiler setting, so
writing the enum itself would make the file format depend on build flags. The
enums are **append-only** — inserting a member reinterprets every world file
ever written. Unknown ordinals are clamped to the inert member (`ctNone` /
`atNone`) on read rather than cast blindly.

The loader reads the 4-byte magic and version prefix *first*, then dispatches to a
version-specific layout. A version 1 file is shorter than a version 2 header, so the
layout must be chosen before reading any further. Versions 2 and 3 share a reader
(`ReadBinaryV2Or3`) because they differ only by the appended Word and the paragraph
section; the version 3 header fields are explicitly zeroed when reading an older file
rather than trusted.

Format validation: Magic signature check, version dispatch, IOResult error handling.

#### Save Games

Save games are a separate file format from world files: magic 'SORS', version **3**,
written by `SaveGameState` / read by `LoadGameState` in datafile.pas. A save stores
position, score, turns, inventory, object and mob placement, and the
visited/taken/talked bitmaps — but no world definition. The header carries a
`WorldSig` fingerprint of the world; restoring a save whose signature does not match,
or whose body length is wrong, is refused outright rather than half-applied. The
expected body length is version-aware: version 1 saves predate the `Talked` bitmap
and are exactly 8 bytes shorter, so they still load with `Talked` zero-filled.

Those three bitmaps are also what gate the first-visit / first-take / first-talk
story paragraphs, so saving them is what stops a restored game replaying scenes.
No separate "paragraphs already seen" state is needed.

Version 3 appends the event runtime state on the same principle — the flag
bitmap, the counters, the `Fired` and `EvEnabled` bitmaps, and **the room
exits**. Exits are world definition everywhere else, but `atLockExit` and
`atUnlockExit` make them mutable, and a locked door that reopened itself on
restore would be worse than no locking at all. Restoring a version 1 or 2 save
resets flags and counters and reseeds `EvEnabled` from what the author wrote,
rather than leaving whatever the current run had mutated them to.

`WorldSignature` deliberately does **not** hash the events. Slot numbers are
stable, so the bitmaps stay meaningful when an author adds or deletes an event,
and every save written before events existed still matches its world.

Two rules the action executor must honour, both save-corruption traps:
`atRemoveObject` and `atRemoveMob` move an entity out of play (`RoomID := 0`,
`CarriedBy := 0`) and **never clear `Active`** — the save writes one state
record per active entity and validates the body length against the current
active count, so clearing `Active` makes every existing save read as truncated.

#### Text Format (Legacy/Manual Editing)

Text-based INI-style format, still fully supported for loading:

```ini
[WORLD]
TITLE=Game Title
START=1
WINROOM=room_id
WINOBJECT=object_id
INTRO=paragraph shown before the first room
WINPARA=paragraph shown on winning
LOSEPARA=paragraph shown when quitting without winning
BOOKLET=0

[ROOM:id]
NAME=Room Name
DESC=Description
NORTH=room_id
SOUTH=room_id
EAST=room_id
WEST=room_id
UP=room_id
DOWN=room_id
POINTS=score awarded on first visit
FIRSTVISIT=paragraph played on first visit

[OBJECT:id]
NAME=Object Name
DESC=Description
ROOM=room_id
CARRIEDBY=mob_id
FLAGS=pickup,use,open,read
USETEXT=Text shown when used
POINTS=score awarded on first take
FIRSTTAKE=paragraph played on first take

[MOB:id]
NAME=Mob Name
DESC=Description
ROOM=room_id
DIALOGUE=What the mob says
FIRSTTALK=paragraph played on first talk

[PARAGRAPH:12]
The body of a paragraph is literal text, not key=value, so it can span
lines and keep its blank lines as paragraph breaks.

Only a real section header ends it — `IsSectionHeader` in datafile.pas
recognises exactly `[WORLD]`, `[ROOM:`, `[OBJECT:`, `[MOB:` and
`[PARAGRAPH:`, so an ordinary body line may begin with a bracket. A body
line that starts with one of those five spellings would be misread; the
binary format has no such ambiguity.
```

#### Format Conversion

Use the converter tool to migrate text worlds to binary:

```bash
make converter
bin/converter input.txt output.dat
```

The game auto-detects format on load (checks for 'SORB' magic signature).

Saving is byte-reproducible: every packed record is `FillChar`-zeroed before it
is populated, so the padding after a short string is zero rather than whatever
was in memory. Converting the same world twice yields identical files, which is
what makes world files diffable and checksummable. **Never `FillChar`
`TGameWorld` itself** — it holds refcounted `AnsiString` paragraph fields.

## Development Tools

Not part of any release; `make tools` and `make test` build them.

| Tool | What it does |
|------|--------------|
| `tools/converter.pas` | Text/BPL world → binary |
| `tools/validate.pas` | Runs `ValidateWorld` from the shell; exits non-zero on errors |
| `tools/bpldump.pas` | Parses a BPL file and prints resolved exits and parse errors — the only way to see VAR resolution without an editor |
| `tools/pairtest.pas` | Unit tests for `PairExits`; run by `make test` and by CI |
| `tools/eventtest.pas` | Unit tests for the event formats and the interpreter; runs natively **and under FreeDOS** |
| `tools/parsetest.pas` | Unit tests for the two-noun parser, `etUseObjectOn` and `etGiveTo`. Native only: it links `GameCore`, so it pulls in `Crt`, which under DOS writes to video memory rather than to a pipe. `make dos32` deliberately does not build it |
| `tools/webformat.js` | Round-trips a world through `web/editor.html`'s own reader and writer under Node, then makes the Pascal agree byte for byte. Run by `make webtest`; the only tool here that is not Pascal |

## Compiler Flags

Free Pascal compiler flags (see Makefile):
- `-O2`: Optimization level 2
- `-XX`: Smart linking (removes unused code)
- `-CX`: Create smartlinkable units
- `-Xs`: Strip symbols from executable
- `-Fu<dir>`: Search directory for units

Target-specific:
- `-Tgo32v2`: DOS 32-bit DPMI (requires CWSDPMI at runtime). Baked into the
  `fpc-go32v2` wrapper the DOS bootstrap writes, along with `-XP` for the djgpp
  binutils and the `-Fu` for the go32v2 units — call the wrapper, not `fpc`
- `-Twin32`: Windows 32-bit
- `-Twin16 -WD`: DOS 16-bit real mode (less common)

The DOS build also passes `-FUbin/dos/units`. go32v2 and native unit files share
their names and differ by architecture, so without a separate output directory
`make native` after `make dos32` would trip over the wrong `.ppu` files.

The tools and tests build through `TOOLFLAGS`, which adds `-FUbin/obj` for the
same class of reason. `bin` is an output directory and so a unit search path,
and `tools/validate.pas` leaves a `validate.o` in it — **Turbo Vision has a
unit of that name**. Build the tools and then `editor-tv` without this and the
linker picks ours, failing with an undefined reference to `TValidator.Valid`
that says nothing at all about the cause.

## Size Constraints

The project must fit on a 720KB (737,280 bytes) floppy disk. CI checks verify this constraint:
- secretorb + editor + world.dat < 720KB
- `make dos-dist` writes the DOS distribution onto a real 720KB FAT12 image, so
  overflowing the disk fails the build instead of printing a warning
- Use size-optimized compiler flags (`-XX`, `-CX`, `-Xs`)
- Minimize world file content in default distribution

## Known Issues

`TODO.md` at the repo root lists known defects found during earlier work and
deliberately left unfixed, with the cost and the fix for each. Check it before
assuming a surprising behaviour is new.

`bin/validate data/world.dat` should always report nothing. The demo world is
a fetch quest — go north twice, take the Glowing Orb, carry it back to the
entrance hall — worth 45 points across two rooms and two objects. CI runs the
validator, so a world change that breaks reachability or the win condition
fails the build rather than shipping.

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

## Editor Programs

The project includes two editor versions:

- **editor.pas**: Lightweight CRT-based editor (included in player distribution)
- **editor-tv.pas**: Professional Turbo Vision editor (for game creators only)

Build commands:
```bash
make native      # Builds secretorb + lightweight editor
make editor-tv   # Builds Turbo Vision editor only
make editors     # Builds both editors
```

There is also **web/editor.html**: a single self-contained HTML file (no build step,
no dependencies, no network access) served from the project site at
`/web/editor.html`. It reads and writes all three world formats, so its byte layout
for binary v4 must stay in step with the packed records in `datafile.pas` — the
record sizes and field offsets are written down in comments next to its
`writeBinary` function, and any drift garbles every record. It also carries a browser
copy of the engine's command handling for playtesting, which mirrors `gamecore.pas`
(including `showParagraph`, the browser twin of `ShowParagraph`, and `fireEvents`,
the twin of `FireEvents` — bounded the same four ways, so a world that misbehaves
in one misbehaves in the other), plus Story and Events tabs and a printable HTML
booklet export. The playtest keeps its own copy of the room exits for the same
reason save version 3 stores them: `LOCKEXIT` makes them mutable, and a test run
must not edit the author's world.

Because the page has no build step and no test framework, that contract is
checked from outside it: `pascal/tools/webformat.js` pulls the model and format
half of the page's script out of the HTML — everything above the
`/* ---- Events ---` comment that begins the DOM wiring — evaluates it under
Node, and round-trips a world through all three formats. `make webtest` then
hands the binary it wrote to `bin/validate` and `bin/converter`: the Pascal
side has to accept it *and* rewrite it byte for byte. That is what makes a
layout drift fail a build rather than corrupt an author's world. `make test`
runs it, and skips with a loud message where Node is not installed — Node is a
test-only dependency and nothing shipped needs it.

The Turbo Vision editor (`editor-tv`) uses Free Pascal's Vision units and provides:
- Menu bar with keyboard shortcuts (F2 Save, F3 Open, Alt+X Exit)
- Dialog-based forms for all entity types
- Scrollable list views with Edit/Delete operations
- Checkbox-based flag editing for objects
- A Story menu with a `PMemo`-based paragraph editor and booklet export. This is the
  only place the `Editors` unit is used; `editor-tv` is excluded from the `dos32`,
  `dos` and `win32` targets, so that dependency never reaches those builds.
- An Events menu. A Turbo Vision dialog cannot relabel itself, so the type is
  chosen first from a picker and the dialog is then *built* for that type,
  asking only for the fields it uses. `PickIndex` is the one picker all three
  enums share. Note that its parameter is `Preselect`, not `Current`:
  `TGroup` has a field of that name and it wins inside a `with Dialog^` block

The lightweight `editor.pas` edits a paragraph as a `MAX_PARA_LINES` × 74 grid of
`ReadLine` calls joined with `#13#10`, because it has no multi-line control. All
three editors can write a booklet whose numbering matches what the game cites.

### Validation, auto-connect and the cross-reference

All three editors share three authoring features, backed by `worldval.pas` on
the Pascal side and by equivalent functions in `web/editor.html`:

| Feature | `editor.pas` | `editor-tv.pas` | `web/editor.html` |
|---------|--------------|-----------------|-------------------|
| Validate world | `V` on the main menu | World ▸ Validate | Check tab |
| Auto-connect exits | prompt on F2-save of a room | prompt on room OK | *Link back* button |
| Paragraph cross-reference | `R` in the paragraph list | Story ▸ Export cross-reference | *Cross-ref* button |
| Event authoring | **read-only list**, `E` on the main menu | Events ▸ List / Add | Events tab |

Event authoring is the one row where the three editors deliberately differ.
`editor.pas` ships on the 720KB floppy beside the game, and a condition and
action list needs more screen and more control than a 25-row `Crt` form has —
so there it is a **read-only list with delete and an enable toggle**, which is
what an author needs while playing with a world on a DOS box: see what exists,
turn one off to bisect a world that misbehaves, remove one. Writing them is
`editor-tv.pas` and `web/editor.html`, neither of which has a size budget.

What that authoring UI is really for is keeping an author from having to know
the encodings. Which of a trigger's two IDs means what changes with the
trigger, and `atLockExit` packs a direction and a destination into one
`SmallInt`. Both editors label their fields from the type in hand — "Object
ID", "Flag number", a direction picker — and the web editor goes further and
makes every entity reference a picker, so a reference that does not exist is a
thing you cannot type rather than something the Check tab tells you about
later. Where a trigger's second ID is one the engine always passes as 0, the
field is hidden rather than shown empty: filling it in would make the event
dead.

The cross-reference is **a separate file from the booklet, deliberately**. The
booklet is what the player is handed; a list of what fires each paragraph would
give the game away. It names every trigger that reaches a paragraph, flags
paragraphs nothing fires, and flags triggers naming an empty slot — the last of
which is invisible at run time, because `ShowParagraph` exits silently on an
empty body.

---

# Editor Enhancement Design Document

This section documents planned enhancements for the Secret Orb editor and game engine. Since the editor is distributed separately from the game runtime, it has no size constraints and can include rich features.

## Design Philosophy

- **Game Runtime**: Must remain small (<720KB total with world file) for retro compatibility
- **Editor**: No size limits; prioritize usability and powerful world-building features
- **Data Format**: Enhancements require updating both the binary format and game engine

## Phase 1: Scripting & Event System (High Priority)

The most critical missing feature for creating engaging text adventures.

### 1.1 Event Triggers

Events fire when specific conditions occur in the game world.

```
TEventTrigger = (
  etEnterRoom,      // Player enters a room
  etExitRoom,       // Player leaves a room
  etFirstVisit,     // First time entering a room
  etTakeObject,     // Player picks up an object
  etDropObject,     // Player drops an object
  etUseObject,      // Player uses an object
  etUseObjectOn,    // Player uses object A on object B/mob/room feature
  etExamineObject,  // Player examines an object
  etTalkToMob,      // Player talks to a mob
  etGiveTo,         // Player gives item to mob
  etTimer,          // Time-based trigger (turns elapsed)
  etFlagSet,        // When a flag becomes true
  etFlagClear       // When a flag becomes false
);
```

### 1.2 Conditions

Conditions gate whether an event's actions execute.

```
TConditionType = (
  ctHasObject,      // Player has object in inventory
  ctObjectInRoom,   // Object is in specific room
  ctMobInRoom,      // Mob is in specific room
  ctFlagIsSet,      // Boolean flag is true
  ctFlagIsClear,    // Boolean flag is false
  ctCounterEquals,  // Counter equals value
  ctCounterGreater, // Counter > value
  ctCounterLess,    // Counter < value
  ctVisitedRoom,    // Player has visited room before
  ctRoomIs,         // Player is in specific room
  ctRandomChance    // Percentage chance (for randomness)
);

TCondition = record
  CondType: TConditionType;
  TargetID: Word;       // Object/Mob/Room/Flag ID
  Value: Integer;       // Comparison value
  Negate: Boolean;      // NOT this condition
end;
```

### 1.3 Actions

Actions modify the game world when triggered.

```
TActionType = (
  atShowMessage,     // Display text to player
  atSetFlag,         // Set boolean flag true
  atClearFlag,       // Set boolean flag false
  atToggleFlag,      // Toggle boolean flag
  atSetCounter,      // Set counter to value
  atAddCounter,      // Add to counter
  atSubCounter,      // Subtract from counter
  atMoveObject,      // Move object to room/inventory/mob
  atRemoveObject,    // Remove object from game (destroy)
  atSpawnObject,     // Create object in room
  atMoveMob,         // Move mob to different room
  atRemoveMob,       // Remove mob from game
  atUnlockExit,      // Enable a room exit
  atLockExit,        // Disable a room exit
  atChangeDesc,      // Change room/object/mob description
  atTeleportPlayer,  // Move player to room
  atAddScore,        // Add to player score
  atEndGame,         // Trigger game ending (win/lose)
  atStartConversation // Begin dialogue tree with mob
);

TAction = record
  ActionType: TActionType;
  TargetID: Word;       // Target object/mob/room/flag
  Value: Integer;       // Numeric value or secondary ID
  TextData: string;     // Message text or new description
end;
```

### 1.4 Event Structure

```
TEvent = record
  ID: Word;
  Name: string[40];           // For editor display
  TriggerType: TEventTrigger;
  TriggerID: Word;            // ID of triggering entity
  TriggerID2: Word;           // Secondary ID (for UseObjectOn)
  Conditions: array[1..4] of TCondition;  // Up to 4 conditions (AND)
  ConditionCount: Byte;
  Actions: array[1..8] of TAction;        // Up to 8 actions
  ActionCount: Byte;
  OneShot: Boolean;           // Fire only once?
  Enabled: Boolean;           // Can be disabled by other events
  Active: Boolean;
end;

const
  MAX_EVENTS = 256;
  MAX_FLAGS = 64;
  MAX_COUNTERS = 32;
```

### 1.5 Example Events

**Locked Door Puzzle:**
```
Event: "Unlock Treasury Door"
  Trigger: etUseObjectOn
  TriggerID: 5 (Golden Key)
  TriggerID2: 12 (Treasury Door)
  Conditions: (none)
  Actions:
    1. atShowMessage "The key turns with a satisfying click."
    2. atSetFlag 1 (treasury_unlocked)
    3. atUnlockExit Room 3, Direction East
    4. atRemoveObject 5 (key consumed)
  OneShot: True
```

**NPC Gives Quest Item:**
```
Event: "Wizard Gives Amulet"
  Trigger: etTalkToMob
  TriggerID: 2 (Old Wizard)
  Conditions:
    1. ctFlagIsSet 5 (completed_wizard_quest)
    2. ctHasObject 10 (Magic Herb) = FALSE [Negate]
  Actions:
    1. atShowMessage "The wizard smiles. 'You have proven worthy...'"
    2. atSpawnObject 15 (Amulet of Power) in player inventory
    3. atSetFlag 6 (has_amulet)
  OneShot: True
```

## Phase 2: Conversation System

Replace single-line mob dialogue with branching conversations.

### 2.1 Dialogue Structure

```
TDialogueNode = record
  ID: Word;
  MobID: Word;                // Which mob this belongs to
  Text: string[255];          // What the NPC says
  Choices: array[1..4] of record
    Text: string[60];         // Player's choice text
    NextNodeID: Word;         // Next dialogue node (0 = end)
    Condition: TCondition;    // Optional condition to show choice
    Actions: array[1..4] of TAction;  // Actions when chosen
    ActionCount: Byte;
  end;
  ChoiceCount: Byte;
  Active: Boolean;
end;

const
  MAX_DIALOGUE_NODES = 512;
```

### 2.2 Example Conversation

```
Node 1 (Merchant, Entry):
  Text: "Welcome, traveler! What brings you to my humble shop?"
  Choices:
    1. "I'd like to buy something." -> Node 2
    2. "I'm looking for information." -> Node 3
    3. "Just browsing." -> Node 4
    4. [If HasFlag quest_active] "About that package..." -> Node 5

Node 3 (Information):
  Text: "Information, eh? What do you want to know?"
  Choices:
    1. "Tell me about the old castle." -> Node 6
    2. "Have you seen any strangers lately?" -> Node 7
    3. "Never mind." -> End
```

## Phase 3: Visual Map Editor

ASCII-based visual representation of the world.

### 3.1 Map Display

```
+-------+       +-------+
| Start |---E---| Hall  |
|  (1)  |       |  (2)  |
+-------+       +---+---+
                    |N
                +---+---+
                |Kitchen|
                |  (3)  |
                +-------+
                    |D
                +---+---+
                |Cellar |
                |  (4)  |
                +-------+
```

### 3.2 Map Features

- Auto-layout using graph algorithms
- Click room to edit
- Drag to reposition (visual only)
- Show unconnected rooms
- Highlight current selection
- Toggle labels (ID, name, both)
- Show one-way vs two-way connections
- Zoom in/out for large worlds

### 3.3 Implementation Notes

- Use a 2D grid array for positioning
- Implement simple force-directed layout
- Store visual positions separately from game data
- Export map as ASCII art for documentation

## Phase 4: Item Enhancements

### 4.1 Object States

```
TObjectState = record
  StateName: string[20];      // "open", "closed", "lit", "broken"
  Description: string[100];   // Description when in this state
  Flags: TObjectFlags;        // Different flags per state
end;

TGameObject = record
  // ... existing fields ...
  States: array[1..4] of TObjectState;
  StateCount: Byte;
  CurrentState: Byte;
end;
```

### 4.2 Container Objects

```
TGameObject = record
  // ... existing fields ...
  IsContainer: Boolean;
  ContainerCapacity: Byte;    // Max items it can hold
  ContainedObjects: array[1..8] of Word;  // Object IDs inside
  ContainedCount: Byte;
  IsLocked: Boolean;
  KeyObjectID: Word;          // Object ID that unlocks it
end;
```

### 4.3 Item Combinations

```
TItemCombination = record
  Object1ID: Word;
  Object2ID: Word;
  ResultObjectID: Word;       // New object created
  ConsumeObject1: Boolean;
  ConsumeObject2: Boolean;
  Message: string[100];
  Active: Boolean;
end;

const
  MAX_COMBINATIONS = 64;
```

## Phase 5: Game Variables

### 5.1 Flag System

```
TGameFlag = record
  ID: Word;
  Name: string[30];           // For editor reference
  Value: Boolean;
  Description: string[60];    // What this flag represents
end;
```

### 5.2 Counter System

```
TGameCounter = record
  ID: Word;
  Name: string[30];
  Value: Integer;
  MinValue: Integer;
  MaxValue: Integer;
  Description: string[60];
end;
```

Common counters: Score, Health, Gold, Turns, Time

## Phase 6: Editor Quality of Life

### 6.1 Validation System

The editor should check for:
- Broken exits (pointing to non-existent rooms)
- Unreachable rooms (no paths from start)
- Dead ends (rooms with only one exit)
- Missing objects (events reference non-existent objects)
- Orphaned dialogue nodes
- Circular event dependencies
- Objects in non-existent rooms

### 6.2 Auto-Connect Rooms

When creating an exit from Room A to Room B:
- Prompt: "Create reverse exit from Room B to Room A?"
- Auto-determine opposite direction

### 6.3 Search & Filter

- Search rooms by name/description
- Filter objects by flags
- Filter mobs by room
- Find all events affecting an entity
- Find all references to a flag/counter

### 6.4 Templates

Pre-defined templates:
- Standard room (4 exits)
- Vertical shaft (up/down only)
- Dead end (1 exit)
- NPC with shop dialogue
- Locked door puzzle
- Container with key

### 6.5 Playtest Mode

- Run game from within editor
- Quick-save editor state
- Test from any room
- Debug view (show flags, counters)
- Step through events

## Phase 7: Extended Descriptions

### 7.1 Conditional Descriptions

```
TConditionalDescription = record
  Condition: TCondition;
  Description: string[255];
  Priority: Byte;             // Higher priority checked first
end;

TRoom = record
  // ... existing fields ...
  BaseDescription: string[255];
  ConditionalDescs: array[1..4] of TConditionalDescription;
  CondDescCount: Byte;
end;
```

### 7.2 Example Uses

- Different description when carrying light source
- Description changes after event (door opens, NPC leaves)
- Time-based descriptions (day/night)
- First visit vs. return visit

## Implementation Priority

### Must Have (Core Features)
1. Event/Scripting system (Phase 1) - Required for puzzles
2. Flags and counters (Phase 5) - Required for events
3. Validation (Phase 6.1) - Prevents broken games

### Should Have (Major Enhancements)
4. Conversation trees (Phase 2) - Rich NPC interactions
5. Object states (Phase 4.1) - Dynamic objects
6. Container objects (Phase 4.2) - Deeper exploration
7. Auto-connect rooms (Phase 6.2) - Editor efficiency

### Nice to Have (Polish)
8. Visual map (Phase 3) - Better visualization
9. Item combinations (Phase 4.3) - Crafting puzzles
10. Conditional descriptions (Phase 7) - Atmosphere
11. Playtest mode (Phase 6.5) - Faster iteration
12. Templates (Phase 6.4) - Quicker content creation

## Data Format Version

When implementing these features, increment the binary format version:
- Version 1: basic rooms, objects, mobs
- Version 2: world title, win condition, per-room and per-object points
- Version 3: story paragraphs, intro and endings, booklet mode
- Version 4 (current): events, flags and counters
- Version 5: Add dialogue trees
- Version 6: Add object states, containers, combinations

The paragraph store is the intended payload for Phase 1's `atShowMessage`: once the
event system arrives, an action can carry a paragraph number instead of inline text,
and the booklet stays a single source of truth for long-form prose.

BPL carries the same fields at `{REVISION:4}`; earlier revisions still load:
- WORLD: `{WINROOM:n}`, `{WINOBJ:n}`, `{INTRO:n}`, `{WINPARA:n}`, `{LOSEPARA:n}`, `{BOOKLET:1}`
- ROOM: `{POINTS:n}`, `{FIRSTVISIT:n}`
- OBJECT: `{POINTS:n}`, `{FIRSTTAKE:n}`
- MOB: `{FIRSTTALK:n}`
- `{START:PARAGRAPH}` blocks with `{OC:n}` and `{TEXT:...}`
- `{START:EVENT}` blocks with `{OC:n}` — the **slot**, not a running index —
  plus `{NAME:}`, `{TRIGGER:}`, `{TRIGGERID:}`, `{TRIGGERID2:}`, `{ONESHOT:}`,
  `{ENABLED:}`, and repeating `{COND:}` / `{ACTION:}` tags. `COND` and `ACTION`
  **accumulate** where every other BPL tag assigns
- WORLD: `{FLAGNAME:n,name}`, `{COUNTERNAME:n,name}`

A `{COND:}` or `{ACTION:}` value is a comma-separated list whose last field —
an action's message — takes everything after the third comma verbatim, so it
may contain commas. It may not contain braces, which would end the tag early;
`EscapeBraces` substitutes them the way `EncodeParaText` does.

A BPL tag value is one line and cannot contain braces, so paragraph line breaks
travel as a `\n` escape (`EncodeParaText`/`DecodeParaText` in bplpars.pas). Paragraphs
are keyed by plain number rather than a `{VAR:Pn}` cross-reference, which keeps the
booklet number the author wrote and sidesteps the unimplemented VAR-resolution pass.

Maintain backward compatibility: newer engine loads older formats, fills defaults.

## File Size Considerations

Estimated data size impact (for a medium game):
- 256 events × ~200 bytes = 50KB
- 64 flags × 40 bytes = 2.5KB
- 32 counters × 50 bytes = 1.6KB
- 512 dialogue nodes × 300 bytes = 150KB
- 64 combinations × 120 bytes = 7.5KB

Total additional data: ~210KB for a content-rich game

This keeps a full game well under the 720KB limit while enabling complex adventures.
