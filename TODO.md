# TODO — known issues awaiting review

Things found while working on the codebase but deliberately left alone, because
they were outside the scope of the change in hand. Each entry says what is wrong,
what it costs today, and what the fix looks like. Line numbers are as of commit
`1b57dce`.

Nothing here is a regression from the story-paragraph work; all of it predates it
except where noted.

---

## 1. `make converter` does not compile

**Where:** `pascal/tools/converter.pas:41`

The converter calls `SaveWorldBinary`, which is private to `datafile.pas`. The
interface exports `SaveWorld` and `SaveWorldAs` (`pascal/src/datafile.pas:16-17`).
The build therefore fails outright.

**Cost:** `CLAUDE.md` documents `make converter` under *Format Conversion* as a
working way to migrate text worlds to binary. It is not. Anyone following the
docs hits a compile error. CI does not build this target, which is why it went
unnoticed.

**Fix:** one line — `SaveWorldAs(OutputFile, World, sfBinary)`. Worth adding the
target to `.github/workflows/pascal.yml` at the same time so it cannot rot again.

---

## 2. World files are not byte-reproducible

**Where:** the packed-record writers in `pascal/src/datafile.pas`

Pascal short strings are a length byte plus a fixed-size payload. Only the used
prefix is assigned, so the padding after each string carries whatever was in the
record's memory beforehand. `BlockWrite` puts that on disk.

**Cost:** saving the same world twice can produce different bytes, so world files
diff noisily and cannot be checksummed. Small fragments of process memory end up
inside `.dat` files that get committed and distributed.

Not a correctness bug: every documented reader takes the length byte as
authoritative, and Pascal↔JS round-trips were verified identical across all three
formats. It is a hygiene and reproducibility problem.

**Fix:** `FillChar` each entity record before populating it. Note the constraint
in `CLAUDE.md`: `FillChar` must never be applied to `TGameWorld` wholesale,
because that record now holds refcounted `AnsiString` paragraph fields. The
packed `*Bin*` structures are plain data and are safe.

---

## 3. `Trim` is declared after its first use

**Where:** `pascal/src/gamecore.pas:181`, used at `:114`, `:123`, `:127`

`ParseCommand` calls `Trim` before the unit's own `Trim` is declared, so those
three calls bind `SysUtils.Trim` while every later call in the unit binds the
local one.

**Cost:** none today — the two behave the same on the inputs involved. It is a
trap: a change to the local `Trim` would silently apply to some call sites and
not others.

**Fix:** move the local `Trim` above `ParseCommand`, or delete it and use
`SysUtils.Trim` throughout.

---

## 4. `LastMessage` is rendered without wrapping

**Where:** `pascal/src/gamecore.pas:401`

The status line goes out through a single `WriteAt(1, CurrentY, G.LastMessage)`.
A message longer than 80 columns relies on the terminal to wrap it, which pushes
the input prompt off the row `ShowRoom` recorded in `TGame.PromptY`.

**Cost:** reachable now — an object description shown by `examine` becomes
`LastMessage` verbatim (`:470`), and descriptions can be long.

**Fix:** cheap since the story-paragraph work landed `WrapText` in `display.pas`.
Wrap into the rows between the message line and the prompt, and either clip or
page the remainder.

---

## 5. BPL `{VAR:...}` references are never resolved

**Where:** `pascal/src/bplpars.pas:612-638`

The second pass is three empty loops with comments explaining that a complete
implementation would resolve VARs here. Exits are stored as raw tag values and
converted with `StrToIntDef`.

**Cost:** a symbolic exit like `{NORTH:R3}` silently becomes `0` — no error, no
warning, just a room with a missing exit. Only numeric IDs actually work, which
makes the VAR feature documented but inert.

Story paragraphs sidestep this deliberately: they are keyed by plain number
rather than `{VAR:Pn}`, which also keeps booklet numbers author-visible.

**Fix:** the symbol table already exists (`:41`). Populate it during the first
pass and resolve in the second, reporting an unresolved VAR as a parse error
rather than silently zeroing it.

---

## 6. `RoomCount` is incremented before the bounds check

**Where:** `pascal/src/bplpars.pas:465-466`

`Inc(W.RoomCount)` runs before the `if CurrentRoom <= MAX_ROOMS` guard on the
next line. A BPL file with more than `MAX_ROOMS` rooms skips the writes but still
leaves `RoomCount` past the end of the array.

**Cost:** every later loop over `1..RoomCount` — including the save path — reads
past the populated entries. Requires a hand-written oversized BPL file to hit.

**Fix:** check first, increment second, and report the overflow instead of
ignoring it.

---

## Verification notes

- The `dos32` and `win32` targets cannot be built locally; those cross-compilers
  are not installed here. CI covers all three platforms and was green on
  `1b57dce` (`build-linux`, `build-windows`, `build-dos32`, `release`).
- `pascal/secorb.pas` is a byte-identical DOS 8.3 duplicate of
  `pascal/secretorb.pas`. Any edit to one must be mirrored, or the FreeDOS build
  silently diverges. Worth a CI check — a `diff` of the two files would do it.
