#!/bin/sh

# What do unix aficianados use to slice and dice in the kitchen? Awk and sed
# indeed!

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

cat "$1" | tr ",;:" '\n' | \
  awk '
  /Game/ { if ($2 > 1) print r, g, b; r=0; g=0; b=0 }
  /red/   { if (r < $1) r = $1 }
  /green/ { if (g < $1) g = $1 }
  /blue/  { if (b < $1) b = $1 }
  '
