# The DOS pipeline

Everything needed to build Secret Orb for DOS on a Linux machine, and to prove
the result actually runs there.

```bash
cd pascal
dos/bootstrap-toolchain.sh   # once: builds the cross-compiler (~1 min)
make dos32                   # bin/dos/*.EXE          (~2 seconds)
make dos-test                # runs them on FreeDOS   (~10 seconds)
make dos-dist                # secretorb-dos32.zip + secretorb-720k.img
```

`BUILD.BAT` in the parent directory is untouched: it is still the way to build
Secret Orb from inside DOS itself, with a DOS-hosted FPC.

## Files

| File | What it does |
|------|--------------|
| `versions.sh` | Every pinned third-party URL and its SHA-256. The single place to bump a version. |
| `common.sh` | `fetch`/`say`/`die`/`need`, sourced by both scripts. |
| `bootstrap-toolchain.sh` | Builds and installs the go32v2 cross-compiler. |
| `run-dos-tests.sh` | Boots the DOS binaries on FreeDOS under QEMU and checks their output. |
| `DISTREAD.TXT` | The README that ships inside the DOS zip and on the floppy image. |

## Why a bootstrap script rather than a package

No distribution ships a go32v2 cross-compiler, and Ubuntu's `fpc` has native
units only — which is why CI used to ship a zip containing nothing but a note
saying the DOS build was unavailable. The script builds one from pinned Free
Pascal sources. Two things about that build are worth knowing:

- **djgpp binutils are required.** The go32v2 RTL has one hand-written startup
  file, `v2prt0.as`, that must be assembled into a COFF-go32 object. Host
  binutils dropped that format, so the pinned djgpp toolchain supplies
  `i586-pc-msdosdjgpp-as`. Only the binutils are kept; its gcc is never called.
- **`Crt` is compiled by hand.** For go32v2 it lives in the `rtl-console`
  package rather than the RTL, and cross-building the package set dies inside
  `fpmake` with a heap overflow. Since `Crt` needs nothing but the RTL, the
  script compiles that one unit directly and skips the packages stage. It is
  not optional: `secorb.pas`, `editor.pas`, `gamecore.pas` and `display.pas`
  all use it.

Everything lands in `$SECRETORB_DOS_TOOLS`, default `~/.cache/secretorb-dos`:
`bin/fpc-go32v2` (a wrapper taking the same arguments as `fpc`), the units, the
binutils, and `CWSDPMI.EXE`.

## What the FreeDOS run actually tests

`run-dos-tests.sh` boots a stock FreeDOS floppy with its language menu and
installer replaced by a batch that runs `TEST.BAT` off the virtual hard disk,
then compares what came back against the native build:

- `PAIRTEST.EXE` — the `PairExits` unit tests, and its output must match the
  Linux run line for line
- `VALIDATE.EXE WORLD.DAT` — the shipped world must be as clean under DOS as it
  is natively
- `CONVERT.EXE` — converting twice must give identical bytes, *and* those bytes
  must equal what the Linux converter produces. World files being
  byte-reproducible only means something if it holds across architectures.

Only the console tools are driven. The game and the editor use the `Crt` unit,
which writes straight to video memory and reads the BIOS keyboard, so no
redirected input can drive them — the same reason `TODO.md` gives for not
testing screen layout down a pipe. To look at the game under DOS, keep the
images and boot them with a display:

```bash
dos/run-dos-tests.sh --keep
qemu-system-i386 -m 32 \
  -drive file=bin/dos/vm/floppy.img,format=raw,if=floppy \
  -drive file=bin/dos/vm/disk.img,format=raw,if=ide -boot a
```

The guest stops itself with `QUITVM.COM`, seven bytes of real mode that write
to QEMU's `isa-debug-exit` port; QEMU then exits with status 1 and the harness
treats anything else as a failure. FreeDOS's own `FDAPM POWEROFF` does nothing
under QEMU, which has no APM, and waiting for the timeout on every run would
cost two minutes instead of ten seconds.

## Bumping a pinned version

Edit the URL in `versions.sh`, download the file by hand, run `sha256sum` on
it, paste the result, then `dos/bootstrap-toolchain.sh --force`. CI keys its
cache on this file, so a bump rebuilds the toolchain there automatically.
