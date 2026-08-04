# TODO — known issues awaiting review

Things found while working on the codebase but deliberately left alone, because
they were outside the scope of the change in hand. Each entry says what is
wrong, what it costs today, and what the fix looks like.

The six defects previously listed here — the broken `make converter`, the
non-reproducible world files, the misbound `Trim`, the unwrapped `LastMessage`,
the inert BPL `{VAR:...}` references and the unchecked `RoomCount` — have all
been fixed. Regression cover for them now lives in CI: the `converter`,
`validate` and `bpldump` targets are built, `tools/pairtest.pas` runs as a unit
test, world files are checked for byte-reproducibility, and `secretorb.pas` is
diffed against its DOS 8.3 duplicate `secorb.pas`.

The unfinishable demo world is fixed too: Shadow Room is now reached north from
Back Room, and carrying the Glowing Orb back to the entrance hall wins.
`bin/validate data/world.dat` reports nothing, and CI now runs it.

---

## 1. `ShowRoom` clips an over-long message instead of paging it

**Where:** `pascal/src/gamecore.pas`, the `LastMessage` block in `ShowRoom`

The message is now wrapped and bounded so the prompt always stays on the row
`PromptY` records. If a message needs more rows than are left on the screen,
`WriteWrappedMax` stops early and the remainder is dropped.

**Cost:** none reachable today. The longest message the engine can produce is a
mob's dialogue — `MAX_DIALOGUE` is 200, about three rows at 78 columns — and
there are typically ten rows free. It would take a much longer field, or a much
taller room description, to hit.

**Fix:** if a future field makes this reachable, hand the overflow to
`ShowTextPage` in `display.pas`, which already pages with a `-- More --`
prompt.

---

## 2. BPL second-pass errors have no line number

**Where:** `pascal/src/bplpars.pas`, the resolve pass in `LoadWorldBPL`

An unresolved `{VAR:...}` is reported as line 0, because by the time the second
pass runs the file has been closed and `CurrentLine` is at EOF. Reporting line 0
is deliberate — the previous behaviour would have blamed the last line of the
file, which is worse — but it is still less help than it could be.

**Cost:** small. The message names the referring room, object or mob and the
direction, which is usually enough to find it.

**Fix:** store the line number alongside each reference in the side tables
(`ExitRefs` and friends) and pass it to `AddError`.

---

## 3. The event system is engine-only so far

**Where:** `pascal/src/events.pas` is written and wired; the authoring and
checking sides are not.

The binary v4 / save v3 / text / BPL formats all carry events, and the
interpreter runs them. All thirteen triggers can now fire — `USE X ON Y` and
`GIVE X TO Y` parse, and `tools/parsetest.pas` covers them — but two pieces of
the design are still missing:

- **`worldval.pas` does not check events**, so an event naming a room that
  does not exist, or an `atShowMessage` with no text and no paragraph, fails
  silently at run time with no symptom — the same class of invisible mistake
  `CheckPara` exists for. `DescribeTriggers` also has to learn about
  `atShowParagraph`, or `WriteParaXRef` reports every event-fired paragraph as
  *"fired by: NOTHING"*.
- **No editor can author an event.** All three read and write them faithfully,
  but the only way to write one today is by hand in the text or BPL format.
  The agreed split when this is picked up: `editor.pas` — the one that ships
  on the floppy — gets a read-only list with delete and an enable toggle, and
  full condition/action authoring goes in `editor-tv.pas` and
  `web/editor.html`, where there is no size budget. `web/editor.html` also
  still needs its binary v4 layout mirrored (the comment block next to
  `writeBinary` is the contract with `datafile.pas`) and a `fireEvents` twin
  for the playtest engine.

**Cost:** a world can use events, but only an author willing to hand-edit
world files, and nothing warns them when they get one wrong.

---

## 4. `atMoveObject` can only move an object to a room

**Where:** `pascal/src/events.pas`, `RunActions`

The action sets `RoomID := Word(Value)` and clears `CarriedBy`, so `Value` is
always a room. The design doc's own wording is "move object to room/inventory/
mob", and its worked example — the wizard handing over an amulet — needs the
inventory case. There is no way to express either today, which is also why
`GIVE` leaves the transfer to the author and the author cannot then write it.

**Cost:** an event can take something away (`atRemoveObject`) or put it on the
floor of the room the player is standing in, which covers most of a hand-over
in practice. It cannot put anything directly into the player's hands.

**Fix:** `Value` is a `SmallInt` and rooms are positive, so the spare space is
the negative half: `-1` for the player's inventory, `-2` and below for a mob
(`MobID = -Value - 1`). The inventory case has to respect `MAX_INVENTORY` and
push onto `PlayerInventory`/`PlayerInvCount`, not just clear `RoomID`. It is an
encoding change to an action that already ships, so it needs a note in the
format documentation and a round-trip test, not a quiet patch.

---

## Verification notes

- The `win32` target cannot be built locally; that cross-compiler is not
  installed here. CI covers it. The `dos32` target now builds anywhere:
  `pascal/dos/bootstrap-toolchain.sh` builds the go32v2 cross-compiler from
  pinned sources, and `make dos-test` runs the result on FreeDOS under QEMU.
- `pascal/src/worldval.pas` is editor-side only. It must never appear in
  `secretorb.pas`'s `uses` clause — the game has no room for authoring checks.
- FPC's CRT unit only emits ANSI positioning when stdout is a terminal, and it
  probes the terminal with `ESC[6n` on startup. Driving the game or the editor
  down a plain pipe therefore tells you nothing about screen layout; a pty that
  answers the probe is needed to test anything positional.
