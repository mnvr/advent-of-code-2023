#!/bin/sh

# What do unix aficianados use to slice and dice in the kitchen? Awk and sed
# indeed!
#
# I don't actually use sed here since the tr is clearer, but you get the vibe.
# If one insists, the equivalent sed would be -- sed 's/[,:;]/\n/g'.

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

cat "$1" | tr ",;:" '\n' | \
  awk '
  /Game/ { if ($2 > 1) print r, g, b; r=0; g=0; b=0 }
  /red/   { if (r < $1) r = $1 }
  /green/ { if (g < $1) g = $1 }
  /blue/  { if (b < $1) b = $1 }
  END { print r, g, b; }
  ' | \
  awk '
  { if ($1 < 13 && $2 < 14 && $3 < 15) c += NR }
  { s += $1 * $2 * $3 }
  END { print c,s }'
