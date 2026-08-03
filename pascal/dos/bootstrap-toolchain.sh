#!/bin/bash
# bootstrap-toolchain.sh - build a go32v2 (DOS 32-bit DPMI) cross-compiler.
#
# Ubuntu's fpc package, and every other distro package, ships native units
# only: there is no go32v2 cross-compiler to install. This script builds one
# from pinned Free Pascal sources and installs it, plus the DPMI host the
# resulting binaries need at run time, under a cache directory.
#
# It is idempotent: with the toolchain already present it does nothing, so the
# Makefile and CI can call it unconditionally.
#
# Usage:
#   dos/bootstrap-toolchain.sh                 build if missing
#   dos/bootstrap-toolchain.sh --force         rebuild from scratch
#   dos/bootstrap-toolchain.sh --print-prefix  print the install prefix, build nothing
#
# Install prefix: $SECRETORB_DOS_TOOLS, or ~/.cache/secretorb-dos.

set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
# shellcheck source=common.sh
. "$HERE/common.sh"     # brings in versions.sh, $PREFIX, $DL, say, die, need, fetch

BUILD=$PREFIX/build
FPCSRC=$BUILD/fpcbuild-$FPC_VERSION/fpcsrc
DJGPP=$PREFIX/djgpp
UNITS=$PREFIX/units/i386-go32v2
WRAPPER=$PREFIX/bin/fpc-go32v2

# The prefix of the djgpp cross binutils inside the pinned tarball. FPC is told
# about it with -XP so it calls i586-pc-msdosdjgpp-as rather than the host as,
# which cannot emit COFF-go32 objects.
BINPREFIX=i586-pc-msdosdjgpp-

FORCE=0
case "${1:-}" in
  --print-prefix) echo "$PREFIX"; exit 0 ;;
  --force)        FORCE=1 ;;
  "")             ;;
  *)              echo "unknown option: $1" >&2; exit 2 ;;
esac

if [ -x "$WRAPPER" ] && [ "$FORCE" -eq 0 ]; then
  echo "go32v2 toolchain already present: $PREFIX"
  echo "(pass --force to rebuild)"
  exit 0
fi

# A host compiler is needed to build the cross compiler. Any FPC of a
# reasonably close version will do; 3.2.2 is what CI installs.
HOSTFPC=${HOSTFPC:-$(command -v fpc || true)}
[ -n "$HOSTFPC" ] || die "no host fpc found. Install Free Pascal first (apt install fpc)."

need curl tar make sha256sum unzip

mkdir -p "$DL" "$BUILD" "$PREFIX/bin"

say "Fetching pinned sources"
fetch "$FPC_URL"     "$FPC_SHA256"     "$DL/fpcbuild-$FPC_VERSION.tar.gz"
fetch "$DJGPP_URL"   "$DJGPP_SHA256"   "$DL/djgpp-$DJGPP_VERSION.tar.bz2"
fetch "$CWSDPMI_URL" "$CWSDPMI_SHA256" "$DL/csdpmi.zip"

say "Unpacking djgpp cross binutils"
# Only the binutils are taken. The gcc in this tarball is never invoked - FPC
# needs an assembler that speaks COFF-go32 and nothing else - so the bulk of
# the 250MB tree is left behind to keep the CI cache small.
rm -rf "$DJGPP" "$BUILD/djgpp-full"
mkdir -p "$BUILD/djgpp-full" "$DJGPP/bin"
tar xjf "$DL/djgpp-$DJGPP_VERSION.tar.bz2" -C "$BUILD/djgpp-full"
DJSRC=$BUILD/djgpp-full/djgpp
for t in as ar ld ld.bfd nm objcopy objdump ranlib strip; do
  cp "$DJSRC/bin/$BINPREFIX$t" "$DJGPP/bin/"
done
# ld reads its default linker scripts from the target lib directory.
mkdir -p "$DJGPP/i586-pc-msdosdjgpp"
cp -r "$DJSRC/i586-pc-msdosdjgpp/lib" "$DJGPP/i586-pc-msdosdjgpp/"
rm -rf "$BUILD/djgpp-full"
export PATH="$DJGPP/bin:$PATH"

say "Unpacking Free Pascal $FPC_VERSION sources"
rm -rf "$BUILD/fpcbuild-$FPC_VERSION"
tar xzf "$DL/fpcbuild-$FPC_VERSION.tar.gz" -C "$BUILD"

# "buildbase" is FPC's own compiler-plus-RTL target: it runs compiler_cycle
# and then the RTL, and stops there. That matters, because the packages stage
# that "crossall" would run next dies inside fpmake with a heap overflow (RTE
# 203) when cross-building - and the only package unit this project needs is
# Crt, compiled by hand further down.
say "Building the go32v2 cross-compiler and RTL (this takes a few minutes)"
make -C "$FPCSRC" buildbase \
  CROSSINSTALL=1 CPU_TARGET=i386 OS_TARGET=go32v2 \
  FPC="$HOSTFPC" BINUTILSPREFIX="$BINPREFIX"

PPCROSS=$FPCSRC/compiler/ppcross386
[ -x "$PPCROSS" ] || die "ppcross386 was not produced"

RTLUNITS=$FPCSRC/rtl/units/go32v2
[ -f "$RTLUNITS/system.ppu" ] || die "the go32v2 RTL was not produced"

say "Installing units"
rm -rf "$UNITS"
mkdir -p "$UNITS"
cp "$RTLUNITS"/*.ppu "$RTLUNITS"/*.o "$UNITS/"

# Crt is not part of the RTL: for go32v2 it lives in the rtl-console package,
# whose normal build goes through fpmake and falls over when cross-building.
# The unit has no dependencies beyond the RTL, so compiling it directly is both
# simpler and faster than fixing the package build.
say "Building the Crt unit (from the rtl-console package)"
CRTSRC=$FPCSRC/packages/rtl-console/src
"$PPCROSS" -Tgo32v2 -Pi386 -O2 -XX -CX -Xs \
  -XP"$BINPREFIX" -Fu"$UNITS" -Fi"$CRTSRC/inc" -FU"$UNITS" \
  "$CRTSRC/go32v2/crt.pp"
[ -f "$UNITS/crt.ppu" ] || die "crt.ppu was not produced"

say "Installing CWSDPMI"
# The DPMI host. A go32v2 program run under plain DOS loads this; under
# Windows or a DPMI-providing memory manager it is not used. cwsdpmi.doc is
# shipped with it, as its licence asks.
rm -rf "$PREFIX/share/cwsdpmi"
mkdir -p "$PREFIX/share/cwsdpmi"
unzip -q -j -o "$DL/csdpmi.zip" "bin/CWSDPMI.EXE" "bin/cwsdpmi.doc" \
  -d "$PREFIX/share/cwsdpmi"

say "Installing compiler"
cp "$PPCROSS" "$PREFIX/bin/ppcross386"

# One wrapper so callers need to know nothing about the layout above. It takes
# the same arguments as fpc.
cat > "$WRAPPER" <<EOF
#!/bin/sh
# Generated by dos/bootstrap-toolchain.sh - do not edit.
# Compiles for DOS 32-bit DPMI (go32v2). Takes the same arguments as fpc.
#
# The djgpp binutils go on PATH because -XP only tells the compiler what to
# call them, not where to find them.
PATH="$DJGPP/bin:\$PATH"
export PATH
exec "$PREFIX/bin/ppcross386" -Tgo32v2 -Pi386 \\
  -XP$BINPREFIX -Fu"$UNITS" "\$@"
EOF
chmod +x "$WRAPPER"

# The build tree is ~1GB of intermediates and is not needed again; the sources
# stay in dl/ so a --force rebuild needs no network.
rm -rf "$BUILD/fpcbuild-$FPC_VERSION"

say "Done"
echo "  prefix   : $PREFIX"
echo "  compiler : $WRAPPER"
echo "  units    : $UNITS"
echo "  cwsdpmi  : $PREFIX/share/cwsdpmi/CWSDPMI.EXE"
