#!/bin/sh
# Pinned third-party artefacts for the DOS pipeline.
#
# Every download is checked against the SHA-256 recorded here, so a moved,
# re-rolled or tampered file fails the build instead of silently producing a
# different toolchain. To move a pin: change the URL and version, download the
# file by hand, run sha256sum on it, and paste the result here.
#
# Sourced by bootstrap-toolchain.sh and run-dos-tests.sh; also hashed into the
# CI cache key, so bumping anything here forces a rebuild.

# Free Pascal sources. The compiler is built from these, cross-targeting
# go32v2. 3.2.2 matches the version CI and the Debian package use natively.
FPC_VERSION=3.2.2
FPC_URL=https://downloads.sourceforge.net/project/freepascal/Source/3.2.2/fpcbuild-3.2.2.tar.gz
FPC_SHA256=85ef993043bb83f999e2212f1bca766eb71f6f973d362e2290475dbaaf50161f

# djgpp cross binutils. FPC's go32v2 RTL has one hand-written startup file
# (v2prt0.as) that must be assembled for COFF-go32; host binutils dropped that
# object format, so this is not optional. Only as/ld/ar/strip are used - the
# gcc in this tarball is never invoked.
DJGPP_VERSION=v3.4
DJGPP_URL=https://github.com/andrewwutw/build-djgpp/releases/download/v3.4/djgpp-linux64-gcc1220.tar.bz2
DJGPP_SHA256=8464f17017d6ab1b2bb2df4ed82357b5bf692e6e2b7fee37e315638f3d505f00

# CWSDPMI, the DPMI host a go32v2 binary needs when DOS provides none. Shipped
# alongside the game; cwsdpmi.doc travels with it to satisfy the licence.
CWSDPMI_VERSION=r7
CWSDPMI_URL=http://www.delorie.com/pub/djgpp/current/v2misc/csdpmi7b.zip
CWSDPMI_SHA256=deacda0488e1cdd7c4a9f32fab45662b34c0ed6b2d7d4d13bc07041b62004a8c

# FreeDOS, used only by run-dos-tests.sh. The Floppy Edition is the smallest
# image that boots to a shell; 144m/x86BOOT.img is the one we want.
FREEDOS_VERSION=1.4
FREEDOS_URL=https://download.freedos.org/1.4/FD14-FloppyEdition.zip
FREEDOS_SHA256=45b1fa7c52dd996c3bfa5e352ffcd410781b952a6ad629f15a4c9ec4bbaefc5a
FREEDOS_BOOT_IMG=144m/x86BOOT.img
