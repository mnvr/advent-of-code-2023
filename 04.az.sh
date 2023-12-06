#!/bin/sh

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

nc=`awk 'END { print NR }' "$1"`

cat "$1" | tr -d '|' | nl | sort -nr | cut -f2- | awk -vnc=$nc '
  {
    matches = 0
    for (i = 3; i <= NF; i++) count[$i] += 1
    for (k in count) if (count[k] > 1) matches += 1
    delete count
    points = 0
    if (matches > 0) points = 2 ^ (matches-1)
    score += points

    i = nc - NR + 1
    for (j = 1; j <= matches; j++) wins[i] += (wins[i+j] + 1)
  }

  END {
    for (i = 1; i <= nc; i++) {
        totalCards += (wins[i] + 1)
    }
    print score "," totalCards
  }
'
