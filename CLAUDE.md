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

The Turbo Vision editor (`editor-tv`) uses Free Pascal's Vision units and provides:
- Menu bar with keyboard shortcuts (F2 Save, F3 Open, Alt+X Exit)
- Dialog-based forms for all entity types
- Scrollable list views with Edit/Delete operations
- Checkbox-based flag editing for objects

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
- Current: Version 1 (basic rooms, objects, mobs)
- Version 2: Add events, flags, counters
- Version 3: Add dialogue trees
- Version 4: Add object states, containers, combinations

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
