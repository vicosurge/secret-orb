#!/bin/bash
# run-dos-tests.sh - run the DOS binaries on real FreeDOS, headless, in QEMU.
#
# Cross-compiling proves the code builds for go32v2. It does not prove the
# result runs: a DPMI host has to load, the RTL has to start up, and file I/O
# has to work through DOS rather than through libc. This boots FreeDOS in QEMU,
# runs the three console tools off a virtual hard disk, pulls their output back
# out with mtools and checks it.
#
# Only the console tools are driven. The game and the editor use the Crt unit,
# which writes straight to video memory and reads the BIOS keyboard, so neither
# can be driven through redirected input - see TODO.md, which makes the same
# point about testing screen layout down a pipe.
#
# Usage: dos/run-dos-tests.sh [--keep]
#   --keep   leave the disk images and captured output in bin/dos/vm

set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
PASCAL=$(cd "$HERE/.." && pwd)
# shellcheck source=common.sh
. "$HERE/common.sh"     # versions.sh, $PREFIX, $DL, say, die, need, fetch

DOSBIN=$PASCAL/bin/dos
NATIVE=$PASCAL/bin
VM=$DOSBIN/vm
OUT=$VM/out
BOOT_TIMEOUT=${BOOT_TIMEOUT:-120}

KEEP=0
[ "${1:-}" = "--keep" ] && KEEP=1

need qemu-system-i386 mtools mformat mpartition mcopy mdir mkfs.msdos curl unzip

for exe in SECORB.EXE EDITOR.EXE VALIDATE.EXE CONVERT.EXE PAIRTEST.EXE WORLD.DAT; do
  [ -f "$DOSBIN/$exe" ] || die "$DOSBIN/$exe is missing - run 'make dos32' first"
done
for tool in converter pairtest; do
  [ -x "$NATIVE/$tool" ] || die "$NATIVE/$tool is missing - run 'make tools test' first"
done

rm -rf "$VM"
mkdir -p "$VM" "$OUT"

# ---------------------------------------------------------------------------
# Boot floppy: stock FreeDOS, with the language menu and the installer replaced
# by a two-line startup that hands straight over to our batch on C:.
# ---------------------------------------------------------------------------
say "Preparing the FreeDOS boot floppy"
fetch "$FREEDOS_URL" "$FREEDOS_SHA256" "$DL/freedos-$FREEDOS_VERSION.zip"
unzip -q -o -j "$DL/freedos-$FREEDOS_VERSION.zip" "$FREEDOS_BOOT_IMG" -d "$VM"
mv "$VM/$(basename "$FREEDOS_BOOT_IMG")" "$VM/floppy.img"

# DOS text files are CRLF; a stray bare LF here is a config line DOS ignores.
crlf() { sed 's/$/\r/' > "$1"; }

crlf "$VM/FDCONFIG.SYS" <<'EOF'
LASTDRIVE=Z
BUFFERS=20
FILES=40
SHELL=\FREEDOS\BIN\COMMAND.COM \FREEDOS\BIN /E:2048 /P=\FDAUTO.BAT
EOF

crlf "$VM/FDAUTO.BAT" <<'EOF'
@ECHO OFF
SET PATH=\FREEDOS\BIN
C:
CD \
IF EXIST C:\TEST.BAT CALL C:\TEST.BAT
REM If TEST.BAT ran to the end it has already stopped the machine. Reaching
REM here means it died early; stop anyway so the run fails on missing output
REM rather than on the timeout.
C:\QUITVM.COM
EOF

mcopy -o -i "$VM/floppy.img" "$VM/FDCONFIG.SYS" ::/FDCONFIG.SYS
mcopy -o -i "$VM/floppy.img" "$VM/FDAUTO.BAT"   ::/FDAUTO.BAT

# ---------------------------------------------------------------------------
# Hard disk: the binaries, the world, and the batch that exercises them.
# ---------------------------------------------------------------------------
say "Building the test disk"

# QUITVM.COM - seven bytes of real mode: write 0 to port 0xF4, return. QEMU's
# isa-debug-exit device turns that into a clean process exit with status 1, so
# the run ends the moment the batch finishes instead of idling until the
# timeout. FreeDOS's own FDAPM POWEROFF does nothing under QEMU (no APM).
#
#   BA F4 00   mov dx, 0xF4
#   B0 00      mov al, 0
#   EE         out dx, al
#   C3         ret
printf '\xBA\xF4\x00\xB0\x00\xEE\xC3' > "$VM/QUITVM.COM"

# ERRORLEVEL is captured the portable way: %ERRORLEVEL% expansion is a
# late addition to FreeCOM, but IF ERRORLEVEL has worked since DOS 2.
crlf "$VM/TEST.BAT" <<'EOF'
@ECHO OFF
C:
CD \
ECHO Running the Secret Orb console tools under FreeDOS
PAIRTEST.EXE > OUT\PAIRTEST.TXT
IF ERRORLEVEL 1 ECHO FAIL > OUT\PAIRTEST.RC
IF NOT ERRORLEVEL 1 ECHO OK > OUT\PAIRTEST.RC
VALIDATE.EXE WORLD.DAT > OUT\VALIDATE.TXT
IF ERRORLEVEL 1 ECHO FAIL > OUT\VALIDATE.RC
IF NOT ERRORLEVEL 1 ECHO OK > OUT\VALIDATE.RC
CONVERT.EXE WORLD.DAT OUT\ROUND1.DAT > OUT\CONVERT.TXT
CONVERT.EXE OUT\ROUND1.DAT OUT\ROUND2.DAT >> OUT\CONVERT.TXT
IF ERRORLEVEL 1 ECHO FAIL > OUT\CONVERT.RC
IF NOT ERRORLEVEL 1 ECHO OK > OUT\CONVERT.RC
ECHO DONE > OUT\DONE.TXT
QUITVM.COM
EOF

# 16MB is far more than the payload needs; it is the smallest size that makes a
# comfortable FAT16 partition. mpartition writes the partition table itself, so
# nothing here needs root or a loop mount.
truncate -s 17M "$VM/disk.img"
cat > "$VM/mtoolsrc" <<EOF
drive c: file="$VM/disk.img" partition=1
EOF
export MTOOLSRC=$VM/mtoolsrc
mpartition -I -c -t 32 -h 16 -s 63 c:
mformat -v SECRETORB c:
mmd c:/OUT
mcopy -o "$DOSBIN"/*.EXE "$DOSBIN/WORLD.DAT" "$VM/QUITVM.COM" "$VM/TEST.BAT" c:/

# ---------------------------------------------------------------------------
say "Booting FreeDOS $FREEDOS_VERSION"
# ---------------------------------------------------------------------------
set +e
timeout "$BOOT_TIMEOUT" qemu-system-i386 \
  -m 32 -display none -no-reboot \
  -device isa-debug-exit,iobase=0xf4,iosize=0x04 \
  -drive file="$VM/floppy.img",format=raw,if=floppy \
  -drive file="$VM/disk.img",format=raw,if=ide \
  -boot a
QEMU_RC=$?
set -e

# isa-debug-exit reports (value << 1) | 1, and QUITVM.COM writes zero.
if [ "$QEMU_RC" -eq 124 ]; then
  die "the guest never stopped within ${BOOT_TIMEOUT}s - it hung or failed to boot"
elif [ "$QEMU_RC" -ne 1 ]; then
  die "qemu exited with $QEMU_RC, expected 1 (the guest's own exit)"
fi

say "Collecting results"
mcopy -o -n "c:/OUT/*" "$OUT/" 2>/dev/null || true

dump_and_die() {
  echo
  echo "--- captured guest output ---"
  for f in "$OUT"/*; do
    [ -f "$f" ] || continue
    echo "--- $(basename "$f")"
    # head is fed rather than piped into: "od | head" would SIGPIPE od, and
    # under pipefail that aborts this function before it reports why.
    case "$f" in
      *.DAT) head -c 64 "$f" | od -An -tx1 ;;
      *)     tr -d '\r' < "$f" ;;
    esac
  done
  echo "-----------------------------"
  die "$1"
}

got() { tr -d '\r' < "$OUT/$1" 2>/dev/null; }

[ -f "$OUT/DONE.TXT" ] || dump_and_die "the test batch did not run to completion"

# --- the unit tests, run under DOS -----------------------------------------
[ "$(got PAIRTEST.RC | tr -d '[:space:]')" = "OK" ] \
  || dump_and_die "PairExits unit tests failed under DOS"
"$NATIVE/pairtest" > "$VM/pairtest-native.txt"
diff -u "$VM/pairtest-native.txt" <(got PAIRTEST.TXT) \
  || dump_and_die "PairExits output under DOS differs from the native run"

# --- the shipped world must be as clean under DOS as it is natively ---------
[ "$(got VALIDATE.RC | tr -d '[:space:]')" = "OK" ] \
  || dump_and_die "validate reported errors in the shipped world under DOS"
if [ "$(got VALIDATE.TXT | grep -c 'error(s)' || true)" -ne 1 ] \
   || ! got VALIDATE.TXT | grep -q '0 issue(s): 0 error(s), 0 warning(s)'; then
  dump_and_die "validate did not report a clean world under DOS"
fi

# --- world files must be byte-reproducible, and the same bytes everywhere ---
[ "$(got CONVERT.RC | tr -d '[:space:]')" = "OK" ] \
  || dump_and_die "the converter failed under DOS"
cmp "$OUT/ROUND1.DAT" "$OUT/ROUND2.DAT" \
  || dump_and_die "converting twice under DOS produced different bytes"
"$NATIVE/converter" "$PASCAL/data/world.dat" "$VM/native.dat" > /dev/null
cmp "$OUT/ROUND1.DAT" "$VM/native.dat" \
  || dump_and_die "the DOS converter and the native converter disagree byte for byte"

say "PASS"
echo "  pairtest : matches the native run"
echo "  validate : shipped world is clean under DOS"
echo "  converter: byte-identical to the native converter"
if [ "$KEEP" -eq 1 ]; then
  echo
  echo "images and captured output kept in $VM"
else
  rm -f "$VM/disk.img" "$VM/floppy.img"
fi
