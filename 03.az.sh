#!/bin/sh

# WIP
# Spam facts about the problem, until we have enough to solve it.

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

cat "$1" | head -2 | tr -s '.' ' ' | awk '
  { print "Row was [" $0 "]"; }
  { for (i=1; i<=NF; i++) print "Field " $i " on row " NR  }
  { for (i=1; i<=NF; i++) if ($i ~ /^[0-9]+$/) print "Number " $i " on row " NR  }
  { for (i=1; i<=NF; i++) if ($i !~ /^[0-9]+$/) print "Symbol " $i " on row " NR  }
' | tee /tmp/$0.tmp.1
