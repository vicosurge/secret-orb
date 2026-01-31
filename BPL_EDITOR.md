# BPL Game Editor — Design Document
### Target: Free Pascal | Scope: Game Editor (Binary Output)

---

## 1. Project Overview

This document defines the architecture, data format specification, and implementation requirements for the **BPL Game Editor** — a standalone desktop application written in Free Pascal that allows a developer to visually create, edit, and validate BPL (Bracket Programming Language) data files. These files define all static game data: rooms, items, creatures, vendors, and vendor lists.

The editor compiles down to a single binary. BPL files are the serialization format it reads and writes. The editor does NOT run the game — it is purely a content creation and validation tool. A future Game Engine document will cover runtime parsing and execution.

---

## 2. BPL Format Specification (Revised)

This section defines the corrected and formalized BPL syntax. Changes from the original design are marked.

### 2.1 Core Syntax Rules

- Every value is wrapped in curly braces: `{KEY:VALUE}`
- Multiple tags can appear on a single line.
- A line ending with `+++` means the declaration continues on the next line. The parser treats everything between the opening tag and the next `{END}` as one continuous object, regardless of line breaks.
- Comments start with `#` and run to the end of the line. The parser ignores them entirely.
- Every object block opens with `{START:TYPE}` and closes with `{END}`. Anything outside these delimiters (other than comments) is a parse error.
- `{REVISION:N}` must be the first tag after `{START}`. This declares which version of BPL syntax the file uses. The parser should reject or warn on unsupported revision numbers.

### 2.2 Value Types

BPL supports four value types. The parser must distinguish between them:

| Type | Syntax | Example | Meaning |
|------|--------|---------|---------|
| Scalar | `VALUE` | `{NAME:Dagger}` | A single static value (string or integer) |
| Range | `(MIN-MAX)` | `{AGE:(18-60)}` | A random integer between MIN and MAX, inclusive. Resolved at runtime by the engine, not by the editor. |
| Set | `(A,B,C)` | `{FACTIONS:(1,2,3,4)}` | A discrete list. One value is chosen at runtime. Resolved by the engine. |
| Dice | `NdM` | `{DAMAGE:1d4}` | Standard dice notation. N dice of M sides. Resolved at runtime. |

**Change from original:** Range and Set previously shared the parenthesis syntax with no formal distinction rule. The rule is now explicit: **parentheses containing a single hyphen between two integers = Range. Parentheses containing commas = Set.** A value like `(1-5)` is a Range. A value like `(1,2,3,4)` is a Set. The parser must enforce this. Mixed syntax such as `(1-5,7,9)` is invalid and should be flagged as an error.

### 2.3 Object Types

#### 2.3.1 ROOM

Defines a traversable location in the game world.

```
{START:ROOM}
{REVISION:1}
{OC:1}{VAR:R1}{NAME:Starter Room}{DESCRIPTION:This is the starter room}+++
{NORTH:TRUE}{NX:R2}{DOOR:NORTH,CLOSED,UNLOCKABLE}+++
{SOUTH:FALSE}{EAST:FALSE}{WEST:FALSE}
{EAST:FALSE}{WEST:FALSE}{UP:FALSE}{DOWN:FALSE}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| OC | Yes | Auto-increment key. Managed by the editor. Developer does not set this manually. |
| VAR | Yes | Variable name used by the engine to reference this object. Format: `R` + integer. |
| NAME | Yes | Display name shown in-game. |
| DESCRIPTION | Yes | In-game description text. |
| NORTH/SOUTH/EAST/WEST/UP/DOWN | Yes | Boolean (TRUE/FALSE). Whether an exit exists in that direction. |
| NW/NE/SW/SE | No | Boolean. Diagonal exits. Only include if used. |
| NX/SX/EX/WX/NWX/NEX/SWX/SEX/UX/DX | Conditional | The VAR of the room this exit leads to. Required if the corresponding direction is TRUE. |
| DOOR | No | Format: `DIRECTION,STATE,PROPERTY`. Direction must match an exit set to TRUE. State is one of: OPEN, CLOSED, LOCKED, BARRED. Property is one of: UNLOCKABLE, BREAKABLE, NONE. Only one DOOR tag per direction. |
| SECRET | No | Boolean. If TRUE, this exit is hidden until a trigger reveals it. The editor should visually distinguish secret exits. |

#### 2.3.2 ITEM

Defines a usable or equippable object.

```
{START:ITEM}
{REVISION:1}
{OC:1}{VAR:I1}{NAME:Dagger}{DESCRIPTION:A simple iron dagger}+++
{SN:1}{DN:3}{IN:1}{DAMAGE:1d4}{SKILL:SHORTSWORDS}+++
{DEFENSE:-1}{DURABILITY:100}{MATERIAL:IRON}{COST:10}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| OC | Yes | Auto-increment key. Managed by the editor. |
| VAR | Yes | Variable name. Format: `I` + integer. |
| NAME | Yes | Display name. |
| DESCRIPTION | Yes | In-game description. |
| SN | Yes | Minimum Strength required to use. Integer. |
| DN | Yes | Minimum Dexterity required. Integer. |
| IN | Yes | Minimum Intelligence required. Integer. |
| DAMAGE | No | Dice notation for damage rolls. Omit for non-weapons. |
| SKILL | No | Skill category required to wield. Omit for non-weapons. |
| DEFENSE | Yes | Defense bonus when wielding. `-1` = no bonus at all. `0` = marginal/unreliable bonus. Positive integers = reliable bonus. |
| DURABILITY | Yes | Integer. Number of hits before the item degrades. |
| MATERIAL | Yes | Material type. Determines what is needed for repair. |
| COST | Yes | Base currency cost. Integer. Economy module may modify this at runtime. |

#### 2.3.3 CREATURE

Defines an NPC. Can also function as a vendor NPC if referenced by a VENDOR block.

```
{START:CREATURE}
{REVISION:1}
{OC:1}{VAR:C1}{NAME:Human Peasant}{RACE:Human}{AGE:(18-60)}+++
{FACTIONS:(1,2,3,4)}{DIALOG:(H1,H2,H3)}+++
{STR:(1-5)}{DEX:(1-5)}{INT:(1-5)}{WEAPONS:ALL}+++
{DIALECT:(Human:3,Draconian:1,Barbarian:0)}{SKILLS:SPOTTING}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| OC | Yes | Auto-increment key. Managed by the editor. |
| VAR | Yes | Variable name. Format: `C` + integer. |
| NAME | Yes | Display name. |
| RACE | Yes | Race of the NPC. Must be from a predefined race list (defined elsewhere). |
| AGE | Yes | Range value. Engine picks a random age within the range at generation time. |
| FACTIONS | Yes | Set value. The engine assigns the NPC to one faction from this set at generation. |
| DIALOG | Yes | Set value. Determines which dialog pool the NPC draws from at runtime. |
| STR | Yes | Range or scalar. Base Strength stat. |
| DEX | Yes | Range or scalar. Base Dexterity stat. |
| INT | Yes | Range or scalar. Base Intelligence stat. |
| WEAPONS | Yes | Weapon restrictions. `ALL` means no restrictions. Otherwise a set of allowed weapon categories. |
| DIALECT | Yes | Language proficiency. Format: `(LANGUAGE:LEVEL,LANGUAGE:LEVEL)`. Level 0 = none, 5 = native/fluent. **Change from original:** Dialect now uses a nested key:value format inside the set to pair each language with a fluency level, rather than a flat list. |
| SKILLS | No | Comma-separated list of skill bonuses this NPC has. |

#### 2.3.4 VENDOR

Defines a vendor instance tied to a location and an NPC.

```
{START:VENDOR}
{REVISION:1}
{OC:1}{VAR:V1}{NAME:Imperial Armory Quartermaster}+++
{TYPE:C1}{LOCATION:R1}{LIST:imperial_standard.bpl}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| OC | Yes | Auto-increment key. Managed by the editor. |
| VAR | Yes | Variable name. Format: `V` + integer. |
| NAME | Yes | Display name. |
| TYPE | Yes | VAR reference to a CREATURE object that acts as this vendor. The editor should validate that this VAR exists and points to a valid CREATURE. |
| LOCATION | Yes | VAR reference to a ROOM where this vendor appears. The editor should validate this. |
| LIST | Yes | Filename of a BPL LIST file containing this vendor's inventory. The editor should validate that this file exists. |

#### 2.3.5 LIST

Defines an inventory or item grouping. Lives in its own BPL file, referenced by VENDOR blocks.

```
{START:LIST}
{REVISION:1}
{ITEM:I5}
{ITEM:I12}
{SEPARATOR}
{ITEM:I3}
{ITEM:I7}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| ITEM | Yes (at least one) | VAR reference to an ITEM object. **Change from original:** LIST now references items by VAR (e.g., `I5`) rather than by display name string. This makes lookups unambiguous and avoids breaking if an item is renamed. |
| SEPARATOR | No | Inserts a visual divider in-game between groups of items. |

#### 2.3.6 EVENT

Defines an event that can occur during gameplay. Events are triggered by the event generation system based on a rolled number that falls within the event's range. Events can be normal occurrences, exceptional situations, epic scenarios (raids, invasions, fires), or unique story-driven moments unlocked by specific triggers.

```
{START:EVENT}
{REVISION:1}
{OC:1}{VAR:E1}{NAME:Quiet Night}{TYPE:NORMAL}+++
{RANGE:(1-30)}{DESCRIPTION:Nothing of note occurs.}+++
{TRIGGER:NONE}{CONSEQUENCE:NONE}
{END}

{START:EVENT}
{REVISION:1}
{OC:65}{VAR:E65}{NAME:Raider Attack}{TYPE:EPIC}+++
{RANGE:(71-90)}{DESCRIPTION:Raiders assault your safe-house!}+++
{TRIGGER:FACTION_WAR:RAIDERS}{CONSEQUENCE:COMBAT_ENCOUNTER:RAIDER_SQUAD}
{END}
```

| Tag | Required | Description |
|-----|----------|-------------|
| OC | Yes | Auto-increment key. Managed by the editor. |
| VAR | Yes | Variable name. Format: `E` + integer. |
| NAME | Yes | Display name of the event. |
| TYPE | Yes | Event tier. One of: NORMAL (1-30), EXCEPTIONAL (31-70), EPIC (71-90), UNIQUE (91-100). The editor should validate that the RANGE matches the TYPE. |
| RANGE | Yes | Range value indicating which die roll results trigger this event. Format: `(MIN-MAX)`. The editor should check for overlapping ranges across events and warn the developer. |
| DESCRIPTION | Yes | The text displayed to the player when this event fires. |
| TRIGGER | No | Prerequisite condition for this event to be available. Format: `CONDITION_TYPE:VALUE`. Common types: NONE (always available), FACTION_WAR:FACTION_NAME, FACTION_PEACE:FACTION_NAME, QUEST_COMPLETED:QUEST_ID, ITEM_OWNED:ITEM_VAR. If omitted, defaults to NONE. |
| CONSEQUENCE | No | What happens when this event resolves. Format: `ACTION_TYPE:VALUE`. Common types: NONE (flavor text only), COMBAT_ENCOUNTER:ENEMY_GROUP, ITEM_GAINED:ITEM_VAR, ITEM_LOST:ITEM_VAR, FACTION_CHANGE:FACTION_NAME:AMOUNT, HP_CHANGE:AMOUNT. Multiple consequences can be chained with semicolons: `COMBAT_ENCOUNTER:RAIDER_SQUAD;FACTION_CHANGE:RAIDERS:-10`. If omitted, defaults to NONE. |

**Editor Notes:** The editor should provide a visual grouping or color-coding of events by TYPE to make it easy to see which tier ranges have coverage. A range overlap checker is strongly recommended — if E5 has RANGE:(10-20) and E8 has RANGE:(15-25), the editor should flag this as a warning, since the overlap creates ambiguity about which event fires when a 15-20 is rolled. The engine would need a resolution rule (first match, last match, random choice), but the editor should help the developer avoid this scenario entirely.

### 2.4 Error Detection

The parser must detect and report the following:

| Error | Description |
|-------|-------------|
| Unclosed bracket | A `{` with no matching `}` on the same tag. Example: `{END` with no closing brace. |
| Missing START or END | An object body with no `{START}` or no `{END}`. |
| Missing REVISION | First tag after START is not REVISION. |
| Invalid value type | A Range containing commas, or a Set containing a hyphen range, or any other mixed syntax. |
| Missing required tag | Any tag marked Required in the tables above is absent. |
| Broken cross-reference | A VAR reference (e.g., in NX, TYPE, LOCATION, or ITEM) that points to an object that does not exist in the loaded project. |
| Duplicate OC | Two objects of the same type share the same OC value. |
| Duplicate VAR | Two objects share the same VAR identifier. |

When an error is detected, the editor should highlight the offending line and display the error in a status panel. It should NOT silently drop or skip objects. A dump/log option for exporting all detected errors to a text file is recommended.

---

## 3. Editor Architecture

### 3.1 Application Structure

The editor is a single binary desktop application. It does not require a runtime, an interpreter, or any external dependencies at execution time. All UI, file I/O, and validation logic is compiled into the executable.

The application is divided into the following layers:

**GUI Layer** — Handles all user-facing windows, panels, forms, and interactions. Built using the Free Pascal `Lazarus` IDE and its `LCL` (Lazarus Component Library) for cross-platform GUI support.

**Editor Logic Layer** — Manages the in-memory state of the current project. Handles object creation, editing, deletion, validation, and cross-reference checking. This layer knows nothing about the GUI — it operates on data structures and returns results.

**Parser/Serializer Layer** — Reads BPL files from disk into internal data structures (parsing). Writes internal data structures back to BPL files (serializing). Handles error detection during parsing. This layer knows nothing about the GUI or the editor logic — it only converts between file text and structured data.

**File I/O Layer** — Handles reading and writing files to disk. Wraps standard Pascal file operations. Provides path resolution relative to the current project directory.

### 3.2 Data Structures

Each BPL object type maps to a Pascal record (or class). These are the internal representations the editor works with. The parser fills them, the editor logic manipulates them, and the serializer writes them back out.

```pascal
{ Room record example }
type
  TDoorInfo = record
    Direction : string;   { NORTH, SOUTH, etc. }
    State     : string;   { OPEN, CLOSED, LOCKED, BARRED }
    Property  : string;   { UNLOCKABLE, BREAKABLE, NONE }
  end;

  TRoom = record
    OC          : integer;
    VarName     : string;          { e.g. 'R1' }
    Name        : string;
    Description : string;
    Exits       : array[0..9] of record
                    Direction : string;  { NORTH, SOUTH, EAST, WEST, NW, NE, SW, SE, UP, DOWN }
                    Enabled   : boolean;
                    TargetVar : string;  { VAR of destination room, empty if not enabled }
                    IsSecret  : boolean;
                  end;
    Doors       : array of TDoorInfo;
  end;
```

Similar records are defined for TItem, TCreature, TVendor, TList, and TEvent. The editor maintains a single project state object that holds dynamic arrays (or TObjectList) of each type, all loaded into memory while the project is open.

### 3.3 Project Structure

A BPL project is a directory on disk. The editor works with one project at a time. Inside the project directory:

```
project_root/
├── rooms/
│   ├── starter_area.bpl
│   └── underground.bpl
├── items/
│   ├── weapons.bpl
│   └── armor.bpl
├── creatures/
│   └── npcs.bpl
├── vendors/
│   ├── vendors.bpl
│   └── imperial_standard.bpl    { This is a LIST file referenced by a vendor }
├── events/
│   ├── normal_events.bpl
│   ├── exceptional_events.bpl
│   ├── epic_events.bpl
│   └── unique_events.bpl
└── project.cfg                  { Project-level config: name, default revision, etc. }
```

A single `.bpl` file can contain multiple objects of the same type (e.g., `weapons.bpl` might contain 10 ITEM blocks). A file should not mix object types — one file, one type. The editor enforces this. LIST files are an exception, as they are referenced by VENDOR blocks by filename and may live alongside vendor definitions.

### 3.4 GUI Layout

The editor uses a standard multi-panel layout:

**Object Browser (left panel)** — A tree view organized by type (Rooms, Items, Creatures, Vendors, Lists, Events). Expanding a type shows all objects of that type currently loaded in the project. Selecting an object loads it into the editor form on the right.

**Editor Form (center/right panel)** — A form with labeled input fields corresponding to every tag in the selected object type. Fields are pre-populated when an existing object is selected. New objects can be created from a blank form. The form layout changes based on which object type is being edited.

**Validation / Status Panel (bottom)** — Displays all errors and warnings for the current project. Errors are listed with their source file, line number (if applicable), and a description. Clicking an error navigates to the relevant object in the browser.

**Menu Bar** — File (New Project, Open Project, Save, Export), Edit (Undo, Redo), Tools (Validate All, Export Error Log), Help.

### 3.5 OC and VAR Management

The `OC` (Order Control) value is an auto-increment integer managed entirely by the editor. The developer never types it. When a new object is created, the editor assigns the next available OC for that type.

The `VAR` is derived from the object type prefix and the OC value. A room with OC 5 gets VAR `R5`. An item with OC 12 gets VAR `I12`. An event with OC 65 gets VAR `E65`. This is automatic. If an object is deleted, its OC and VAR are retired — they are not reused, to avoid breaking references in other files. The editor should warn if a deleted object's VAR is still referenced elsewhere.

---

## 4. Workflow

A typical developer workflow in the editor looks like this:

1. **Open or create a project.** The editor loads all `.bpl` files in the project directory and parses them into memory. Any parse errors are displayed immediately in the status panel.

2. **Browse existing objects.** The object browser populates with everything that was loaded. The developer can click through rooms, items, creatures, and so on to review what exists.

3. **Create a new object.** The developer selects a type in the browser, clicks "New," fills in the form fields, and saves. The editor assigns OC and VAR automatically and adds the object to the appropriate file (or creates a new file if the developer specifies one).

4. **Edit an existing object.** Select it in the browser, modify fields in the form, save. The editor updates the in-memory state and marks the file as dirty.

5. **Validate.** At any point the developer can run a full validation pass. The editor checks all objects for missing required fields, broken cross-references, invalid value types, and structural errors. Results appear in the status panel.

6. **Save.** The editor serializes all dirty objects back to their source `.bpl` files. It preserves comments that existed in the original file where possible. Formatting follows the conventions defined in Section 2.1.

---

## 5. Implementation Priorities

This is the recommended build order. Each phase produces a working, testable milestone.

**Phase 1 — Parser and Serializer.** Build the parser that reads `.bpl` files and populates internal records. Build the serializer that writes records back to valid `.bpl` syntax. Build error detection for structural issues (unclosed brackets, missing START/END, missing REVISION). Test with hand-written `.bpl` files. No GUI required at this stage — a simple command-line test harness is sufficient.

**Phase 2 — Validation Engine.** Add value-type validation (Range vs Set syntax enforcement). Add required-field checking for all object types. Add cross-reference validation (does this VAR actually exist?). Add duplicate OC/VAR detection. Validation runs on the full in-memory project state, not on individual files.

**Phase 3 — Core GUI.** Build the LCL application skeleton: main window, object browser tree, editor form panel, and status panel. Wire the object browser to the in-memory project state. Wire object selection to form population. Implement New, Edit, Save, and Delete for at least one object type (ITEM is recommended as a starting point — it has no cross-references to other types).

**Phase 4 — Full Object Type Support.** Extend the editor form to handle all six object types (ROOM, ITEM, CREATURE, VENDOR, LIST, EVENT). Implement the dynamic form layout that changes based on which type is selected. Add cross-reference pickers where needed (e.g., selecting a target room for an exit, selecting a creature for a vendor's TYPE field). For EVENT objects, implement the range overlap checker and TYPE-to-RANGE validation.

**Phase 5 — Polish and Error UX.** Wire the validation results into the status panel with clickable navigation. Implement the error log export. Add undo/redo. Add project-level save/load. Test the full create → edit → validate → save → reload cycle end to end.

---

## 6. Out of Scope

The following are explicitly not part of this document and are deferred to a future Game Engine design document:

- Runtime resolution of Range, Set, and Dice values.
- The event generation system's **runtime mechanics** — specifically, the dice rolling formula (`%fdr% +/- %clck% + %specn% = %eventn%`), the luck modifier system, faction status modifiers, and the actual triggering of events during gameplay or sleep cycles. The **editor** handles creating and validating EVENT objects, but the **engine** handles rolling for them and executing their consequences.
- The time system and sleep loop.
- Faction meter tracking and per-town state.
- Actual gameplay logic of any kind.
- BPL file encryption (mentioned in the original design as a future addition).

---

*Document version: 1.1 | Prepared for use as a Claude Code implementation prompt.*

