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

---

## 1. The shipped demo world cannot be completed

**Where:** `pascal/data/world.dat`

`bin/validate data/world.dat` reports:

```
warn  [world] No win room is set, so the adventure cannot be completed.
ERROR [room 3] Room "Shadow Room" cannot be reached from the start room.
```

Both are real. Room 3 has all six exits set to 0 and no other room points at
it, so it is unreachable content; and with no `WINROOM` the game has no ending
short of quitting.

**Cost:** the demo that ships on the floppy is unfinishable, and a third of its
rooms are dead. Anyone using it as a worked example is copying a broken world.

**Fix:** a content decision, not a code one — connect Shadow Room to Back Room
(or delete it) and set `WINROOM`, probably with `WINOBJECT` pointing at the
Rusty Key. Left alone here because changing the shipped world is a design call.

---

## 2. `ShowRoom` clips an over-long message instead of paging it

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

## 3. BPL second-pass errors have no line number

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

## Verification notes

- The `dos32` and `win32` targets cannot be built locally; those cross-compilers
  are not installed here. CI covers all three platforms.
- `pascal/src/worldval.pas` is editor-side only. It must never appear in
  `secretorb.pas`'s `uses` clause — the game has no room for authoring checks.
- FPC's CRT unit only emits ANSI positioning when stdout is a terminal, and it
  probes the terminal with `ESC[6n` on startup. Driving the game or the editor
  down a plain pipe therefore tells you nothing about screen layout; a pty that
  answers the probe is needed to test anything positional.
