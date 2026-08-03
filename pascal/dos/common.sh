#!/bin/bash
# Shared helpers for the DOS pipeline scripts. Sourced, never run directly.

# shellcheck source=versions.sh
. "$(dirname "${BASH_SOURCE[0]}")/versions.sh"

# Where the toolchain, the pinned downloads and CWSDPMI live. Overridable so a
# build machine can put them somewhere other than the home directory.
PREFIX=${SECRETORB_DOS_TOOLS:-$HOME/.cache/secretorb-dos}
DL=$PREFIX/dl

say() { printf '\n=== %s\n' "$*"; }
die() { echo "ERROR: $*" >&2; exit 1; }

need() {
  for t in "$@"; do
    command -v "$t" >/dev/null || die "$t is required but not installed"
  done
}

# fetch <url> <sha256> <destfile>
#
# Downloads only what is missing, but verifies the checksum every time: a
# truncated, swapped or tampered artefact must never reach a build.
fetch() {
  local url=$1 want=$2 dest=$3 got
  mkdir -p "$(dirname "$dest")"
  if [ ! -f "$dest" ]; then
    echo "downloading $(basename "$dest")"
    curl -fsSL --retry 3 -o "$dest.part" "$url" || die "download failed: $url"
    mv "$dest.part" "$dest"
  fi
  got=$(sha256sum "$dest" | cut -d' ' -f1)
  if [ "$got" != "$want" ]; then
    echo "  expected $want" >&2
    echo "  actual   $got" >&2
    echo "  (if the upstream file legitimately changed, update dos/versions.sh)" >&2
    die "checksum mismatch for $dest"
  fi
}
