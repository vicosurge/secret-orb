#!/bin/bash
# Secret Orb build script
# Usage: ./build.sh [target]
# Targets: native (default), dos, dos32, win32, clean

set -e

SRCDIR="src"
BINDIR="bin"
DATADIR="data"

FPC="fpc"
FPCFLAGS="-O2 -XX -CX -Xs -Fu${SRCDIR}"

mkdir -p "$BINDIR"

case "${1:-native}" in
    native)
        echo "Building for current platform..."
        $FPC $FPCFLAGS -o"${BINDIR}/secretorb" secretorb.pas
        $FPC $FPCFLAGS -o"${BINDIR}/editor" editor.pas
        cp "${DATADIR}/world.dat" "${BINDIR}/"
        echo "Build complete. Binaries in ${BINDIR}/"
        ;;
    dos32)
        echo "Building for DOS (32-bit DPMI)..."
        $FPC $FPCFLAGS -Tgo32v2 -o"${BINDIR}/secretorb.exe" secretorb.pas
        $FPC $FPCFLAGS -Tgo32v2 -o"${BINDIR}/editor.exe" editor.pas
        ;;
    win32)
        echo "Building for Windows 32-bit..."
        $FPC $FPCFLAGS -Twin32 -o"${BINDIR}/secretorb.exe" secretorb.pas
        $FPC $FPCFLAGS -Twin32 -o"${BINDIR}/editor.exe" editor.pas
        ;;
    clean)
        echo "Cleaning..."
        rm -rf "$BINDIR"
        rm -f ${SRCDIR}/*.o ${SRCDIR}/*.ppu
        rm -f *.o *.ppu
        echo "Clean complete."
        ;;
    *)
        echo "Unknown target: $1"
        echo "Usage: $0 [native|dos32|win32|clean]"
        exit 1
        ;;
esac

# Show binary sizes
if [ -d "$BINDIR" ] && [ "$1" != "clean" ]; then
    echo ""
    echo "Binary sizes:"
    ls -lh "${BINDIR}/"* 2>/dev/null || true
fi
