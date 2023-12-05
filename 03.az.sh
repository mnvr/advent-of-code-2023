#!/bin/sh

# WIP
# Spam facts about the problem, until we have enough facts to solve it.

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1
head -1 "$1" > /dev/null || exit 1

echo "Hello Dear May," | tee /tmp/ac3.facts

read_log () { cp /tmp/ac3.facts /tmp/ac3.facts.old; cat /tmp/ac3.facts.old; }
log () { tee -a /tmp/ac3.facts; }

cat "$1" | awk '
  {
    split($0, cs, "")
    for(i=1; i<=length($0); i++) {
      c = cs[i]
      if (c ~ /[0-9]/) {
        n = n  c
        if (start==0) { start = i }
      } else {
        if (start) {
          print "Number " n " on row " NR " and column " start;
          n = ""
          start = 0
        }
        if (c != ".") {
          print "Symbol " c " on row " NR " and column " i
        }
      }
    }
  }
' | log

read_log | awk '/Number/ { print $2, $5, $8 }' |
while read n r c
do
  nsym=$(cat "$1" | awk -vn=$n -vr=$r -vc=$c '
    NR == r-1 || NR == r || NR == r + 1 {
      print substr($0,c-1,length(n)+2)
    }
  ' | tr -d '0-9.\n' | wc -c | tr -d ' ')
  echo "Number $n on row $r and column $c has $nsym symbols around it" \
    | log
  if test "$nsym" -ne 0
  then
    echo "Part $n on row $r and column $c" \
    | log
  fi
done

read_log | awk '/Part/ { s += $2 }
  END { print "The sum of all part numbers is " s }' | log

rm /tmp/ac3.facts.old   #  One must keep flowing, like water.

echo "Yours Truly," | log
