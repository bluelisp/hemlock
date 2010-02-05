#!/bin/sh -e
set -x

# use this script to set up a binary distribution of hemlock.
#
# Since the binary is not relocatable at the moment, you need to
# build it in the place where it would be extracted on the user's system. 
#
# 1. Make a directory /opt/hemlock
#
# 2. Check out clbuild to /opt/hemlock/clbuild
# 2a. (Optionally) Install Qt libraries into /opt/hemlock/lib
#
# 3. Use clbuild to download hemlock
#
# 4. Use clbuild to download SBCL, patch it using sbcl.diff and build it
#
# 5. Run this script
#
# 6. Find tarballs in /opt/hemlock
#
# - Only the -base- tarballs is required for users.
# - The optional -qt- tarball extracts on top of the -base- tarball
#   and enables use of the qt backend.
# - The optional -src- tarball enabled use of M-.
#

base=/opt/hemlock
ver=$(date '+%Y-%m-%d')-$(cd $base/clbuild/source/hemlock && git show-ref --hash=8 HEAD)
export PATH=$base/clbuild:$PATH

cd $base/clbuild/source/hemlock
./build.sh tty qt clx
cp hemlock $base/

cd $base

tar cjf hemlock-bin-base-$ver.tar.bz2 \
	--absolute-names \
	--exclude '*/sbcl.core' \
	$base/hemlock \
	$base/clbuild/source/iolib/src/syscalls/libiolib-syscalls.so \
	$base/clbuild/source/osicat/posix/libosicat.so \
	$base/clbuild/target/lib/sbcl \
        $base/clbuild/source/hemlock/resources/hemlock11.cursor

tar cjf hemlock-bin-qt-$ver.tar.bz2 \
	--absolute-names \
	$base/background.svg \
	$base/clbuild/source/commonqt/libcommonqt.so* \
	$base/lib

tar cjf hemlock-src-$ver.tar.bz2 \
	--absolute-names \
	--exclude '*/sbcl.core' \
	--exclude '*/source/hemlock/hemlock' \
	--exclude '*/source/sbcl/obj/*' \
	--exclude '*/source/sbcl/output/*' \
	--exclude '*.fasl' \
	--exclude '*/clbuild/target/*' \
	$base/clbuild
