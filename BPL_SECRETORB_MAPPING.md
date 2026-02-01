# BPL to Secret Orb Mapping

This document defines how BPL (Bracket Programming Language) syntax maps to Secret Orb's data structures. BPL serves as the human-readable source format, which compiles to the binary SORB format for the game runtime.

## Design Principles

1. **BPL is the source format** - Developers edit `.bpl` files
2. **Binary is the runtime format** - Game loads `.dat` files with SORB magic
3. **Editor handles conversion** - Load BPL, save binary (or both)
4. **Simplified BPL** - Only features Secret Orb supports are included

---

## Object Type Mapping

### WORLD (New for BPL)

Defines global world properties. One per project.

**BPL Syntax:**
```
{START:WORLD}
{REVISION:1}
{TITLE:The Secret Orb}{START:R1}
{END}
```

**Maps to:** `TGameWorld.Title` and `TGameWorld.CurrentRoom`

| BPL Tag | Required | Secret Orb Field | Notes |
|---------|----------|------------------|-------|
| REVISION | Yes | (parser version) | Must be 1 |
| TITLE | Yes | TGameWorld.Title | Max 40 chars |
| START | Yes | TGameWorld.CurrentRoom | VAR reference (R1, R2, etc.) |

---

### ROOM

Defines a traversable location.

**BPL Syntax:**
```
{START:ROOM}
{REVISION:1}
{OC:1}{VAR:R1}{NAME:Entrance Hall}+++
{DESC:A grand entrance with marble floors and tall columns.}+++
{NORTH:R2}{SOUTH:0}{EAST:R3}{WEST:0}{UP:0}{DOWN:0}
{END}
```

**Maps to:** `TRoom` record

| BPL Tag | Required | Secret Orb Field | Notes |
|---------|----------|------------------|-------|
| OC | Yes | TRoom.ID | Auto-managed by editor |
| VAR | Yes | (cross-reference) | Format: R + OC |
| NAME | Yes | TRoom.Name | Max 40 chars |
| DESC | Yes | TRoom.Desc | Max 255 chars |
| NORTH | Yes | TRoom.Exits[dirNorth] | VAR ref or 0 |
| SOUTH | Yes | TRoom.Exits[dirSouth] | VAR ref or 0 |
| EAST | Yes | TRoom.Exits[dirEast] | VAR ref or 0 |
| WEST | Yes | TRoom.Exits[dirWest] | VAR ref or 0 |
| UP | Yes | TRoom.Exits[dirUp] | VAR ref or 0 |
| DOWN | Yes | TRoom.Exits[dirDown] | VAR ref or 0 |

**Notes:**
- Exit values are VAR references (e.g., `R2`) or `0` for no exit
- Parser resolves VAR to numeric ID
- BPL's diagonal directions (NW, NE, SW, SE) are not supported
- BPL's DOOR and SECRET tags are reserved for future use

---

### OBJECT

Defines an interactive item (mapped from BPL's ITEM type).

**BPL Syntax:**
```
{START:OBJECT}
{REVISION:1}
{OC:1}{VAR:O1}{NAME:Rusty Key}+++
{DESC:An old iron key covered in rust.}+++
{ROOM:R1}{CARRIEDBY:0}{FLAGS:pickup,use}+++
{USETEXT:You insert the key into the lock.}
{END}
```

**Maps to:** `TGameObject` record

| BPL Tag | Required | Secret Orb Field | Notes |
|---------|----------|------------------|-------|
| OC | Yes | TGameObject.ID | Auto-managed |
| VAR | Yes | (cross-reference) | Format: O + OC |
| NAME | Yes | TGameObject.Name | Max 30 chars |
| DESC | Yes | TGameObject.Desc | Max 100 chars |
| ROOM | Yes | TGameObject.RoomID | VAR ref or 0 (inventory) |
| CARRIEDBY | No | TGameObject.CarriedBy | MOB VAR ref or 0 |
| FLAGS | No | TGameObject.Flags | Comma-separated |
| USETEXT | No | TGameObject.UseText | Max 100 chars |

**Flag Values:**
- `pickup` - Player can take this object (ofPickup)
- `use` - Player can use this object (ofUse)
- `open` - Player can open this object (ofOpen)
- `read` - Player can read this object (ofRead)

**Notes:**
- BPL ITEM stats (SN, DN, IN, DAMAGE, SKILL, etc.) are not mapped
- Those are reserved for future RPG expansion

---

### MOB

Defines an NPC (mapped from BPL's CREATURE type).

**BPL Syntax:**
```
{START:MOB}
{REVISION:1}
{OC:1}{VAR:M1}{NAME:Old Wizard}+++
{DESC:A wizened old man in tattered robes.}+++
{ROOM:R1}{DIALOGUE:Greetings, traveler. Seek the orb in the eastern caves.}
{END}
```

**Maps to:** `TMob` record

| BPL Tag | Required | Secret Orb Field | Notes |
|---------|----------|------------------|-------|
| OC | Yes | TMob.ID | Auto-managed |
| VAR | Yes | (cross-reference) | Format: M + OC |
| NAME | Yes | TMob.Name | Max 30 chars |
| DESC | Yes | TMob.Desc | Max 100 chars |
| ROOM | Yes | TMob.RoomID | VAR reference |
| DIALOGUE | No | TMob.Dialogue | Max 200 chars |

**Notes:**
- BPL CREATURE stats (RACE, AGE, FACTIONS, STR, DEX, INT, etc.) are not mapped
- DIALOGUE replaces BPL's DIALOG set (single string, not pool)

---

## Syntax Adaptations

### Simplified Value Types

BPL defines Range `(18-60)`, Set `(1,2,3)`, and Dice `1d4` value types. For Secret Orb v1:

- **Scalar values only** - All values are static strings or integers
- Runtime-resolved types are reserved for future expansion

### Cross-Reference Format

| Entity Type | VAR Prefix | Example |
|-------------|------------|---------|
| Room | R | R1, R2, R15 |
| Object | O | O1, O2, O30 |
| Mob | M | M1, M2, M10 |

The parser maintains a symbol table to resolve VAR references to numeric IDs.

### Line Continuation

Lines ending with `+++` continue on the next line. The parser joins them before processing tags.

### Comments

Lines starting with `#` are comments and ignored by the parser.

---

## File Structure

### Single-File Format

For simplicity, Secret Orb BPL files contain all objects in one file:

```
# world.bpl - Secret Orb World Definition
# Generated by Secret Orb Editor

{START:WORLD}
{REVISION:1}
{TITLE:The Secret Orb}{START:R1}
{END}

{START:ROOM}
{REVISION:1}
{OC:1}{VAR:R1}{NAME:Forest Clearing}+++
{DESC:A peaceful clearing surrounded by ancient oaks.}+++
{NORTH:R2}{SOUTH:0}{EAST:0}{WEST:0}{UP:0}{DOWN:0}
{END}

{START:ROOM}
{REVISION:1}
{OC:2}{VAR:R2}{NAME:Dark Cave}+++
{DESC:A damp cave entrance. Water drips from above.}+++
{NORTH:0}{SOUTH:R1}{EAST:0}{WEST:0}{UP:0}{DOWN:R3}
{END}

{START:OBJECT}
{REVISION:1}
{OC:1}{VAR:O1}{NAME:Torch}+++
{DESC:A wooden torch wrapped in oil-soaked rags.}+++
{ROOM:R1}{FLAGS:pickup,use}+++
{USETEXT:The torch flickers to life, casting dancing shadows.}
{END}

{START:MOB}
{REVISION:1}
{OC:1}{VAR:M1}{NAME:Hermit}+++
{DESC:A weathered old man with knowing eyes.}+++
{ROOM:R1}{DIALOGUE:The orb lies deep below. Beware the guardian.}
{END}
```

### Multi-File Format (Future)

For larger projects, BPL supports splitting by type:
```
project/
  world.bpl      # WORLD block only
  rooms.bpl      # All ROOM blocks
  objects.bpl    # All OBJECT blocks
  mobs.bpl       # All MOB blocks
```

The editor can merge these on load and split on save.

---

## Parser Error Handling

The parser must detect and report:

| Error | Description |
|-------|-------------|
| `E001` | Unclosed brace - `{TAG` without `}` |
| `E002` | Missing START - Object body without `{START:TYPE}` |
| `E003` | Missing END - Object not closed with `{END}` |
| `E004` | Missing REVISION - First tag after START is not REVISION |
| `E005` | Unknown type - `{START:UNKNOWN}` |
| `E006` | Missing required tag - Required field not present |
| `E007` | Invalid VAR format - VAR doesn't match `[ROMW][0-9]+` |
| `E008` | Broken reference - VAR points to non-existent object |
| `E009` | Duplicate OC - Two objects of same type share OC |
| `E010` | Duplicate VAR - Two objects share same VAR |
| `E011` | Value too long - String exceeds max length |

---

## Conversion Pipeline

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  .bpl file  │────>│   Parser    │────>│ TGameWorld  │
│  (source)   │     │ (bplparser) │     │  (memory)   │
└─────────────┘     └─────────────┘     └──────┬──────┘
                                               │
                    ┌─────────────┐     ┌──────▼──────┐
                    │  .dat file  │<────│  Serializer │
                    │  (binary)   │     │  (datafile) │
                    └─────────────┘     └─────────────┘
```

The editor can also serialize back to BPL for round-trip editing.

---

## Future Extensions

These BPL features are documented but not implemented in v1:

1. **Events** - `{START:EVENT}` with triggers and actions
2. **Flags/Counters** - Game state variables
3. **Dialogue Trees** - Multi-node conversations
4. **Object States** - Dynamic object descriptions
5. **Containers** - Objects holding other objects
6. **Runtime Values** - Range, Set, Dice resolution

Each extension will increment the REVISION number when implemented.

---

*Document version: 1.0 | For Secret Orb BPL Integration*
