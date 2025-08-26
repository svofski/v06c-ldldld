#!/bin/bash

set -x

BASE=9000
CORE=ldldld.$BASE
CORE_Z=ldldld.zx0

dd if=ldldld.com of=$CORE skip=$[0x8f00] bs=1
./tools/salvador.exe -classic $CORE $CORE_Z

./TASM.EXE -b -85 -DBASE=0${BASE}h start.asm start.0100

cat start.0100 $CORE_Z >ld.com

../fddutil/fddutil.js -i ld.com -i rogue06c.com -i rogue.doc -o ldldld.fdd
