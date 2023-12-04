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

rm /tmp/$0.tmp.2
cat /tmp/$0.tmp.1 | awk '/Number/ { print $2, $5 }' | while read n r
do
  echo "Looking for $n from row $r"
  cat "$1" | grep --word $n | awk -vn=$n -vr=$r '
    { print "Number " n " on row " r " and column " index($0, n) }
  ' | tee -a /tmp/$0.tmp.2
done

rm /tmp/$0.tmp.3
cat /tmp/$0.tmp.1 | awk '/Symbol */ { print $5 }' | while read r
do
  echo "Looking for * from row $r"
  cat "$1" | awk -vr=$r '
    NR == r { print "Symbol * on row " r " and column " index($0, "*") }
  ' | tee -a /tmp/$0.tmp.3
done

rm /tmp/$0.tmp.4
cat /tmp/$0.tmp.2 | grep column | awk '/Number/ { print $2, $5, $8 }' \
| while read n r c
do
  echo "Is the $n on row $r and column $c a part?"
  cat "$1" | awk -vn=$n -vr=$r '
    NR == r-1 || NR == r || NR == r + 1 {
      print NR, $0, "Number " n " on row " r " and column " index($0, n) }
  ' | tee -a /tmp/$0.tmp.4
done
