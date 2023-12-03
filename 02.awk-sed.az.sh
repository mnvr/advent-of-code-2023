#!/bin/sh

# What do unix aficianados use to slice and dice in the kitchen? Awk and sed
# indeed!

cat examples/02 | head -2 | tr ",;:" '\n' | \
#   awk '/Game/ { if (NR != 1) print $r, $g, $b; r=0; g=0; b=0; } '
#   awk '/Game/ { if (NR > 1) print NR }'
  awk '/Game/ { if ($2 > 1) print r, g, b; r=0;g=0;b=0 }'
