#!/bin/sh

# WIP
# Spam facts about the problem, until we have enough to solve it.

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

echo "Hello Dear May," | tee /tmp/ac3.facts

cat "$1" | awk -F '[^0-9]' '
  { for (i=1; i<=NF; i++) { if ($i != "") print "Number " $i " on row " NR; } }
' | tee -a /tmp/ac3.facts

cat "$1" | tr '.' ' ' | tr -d '0-9' | awk '
  { for (i=1; i<=NF; i++) { if ($i != "") print "Symbol " $i " on row " NR; } }
' | tee -a /tmp/ac3.facts

cp /tmp/ac3.facts /tmp/ac3.facts.old
cat /tmp/ac3.facts.old | awk '/Number/ { print $2, $5 }' | while read n r
do
  echo "Looking for $n from row $r"
  cat "$1" | awk -vn=$n -vr=$r '
    NR == r { print "Number " n " on row " r " and column " index($0, n) }
  ' | tee -a /tmp/ac3.facts
done

cp /tmp/ac3.facts /tmp/ac3.facts.old
cat /tmp/ac3.facts.old | awk '/Symbol */ { print $5 }' |
while read r
do
  echo "Looking for * from row $r"
  cat "$1" | awk -vr=$r '
    NR == r { s = $0; while (1) {
      i = index(s, "*");
      if (i == 0) break;
      c += i;
      print "Symbol * on row " r " and column " c;
      s = substr(s, c + 1);
    } }
  ' | tee -a /tmp/ac3.facts
done

cp /tmp/ac3.facts /tmp/ac3.facts.old
cat /tmp/ac3.facts.old | grep column | awk '/Number/ { print $2, $5, $8 }' |
while read n r c
do
  nsym=$(cat "$1" | awk -vn=$n -vr=$r -vc=$c '
    NR == r-1 || NR == r || NR == r + 1 {
      print substr($0,c-1,length(n)+2)
    }
  ' | tr -d '0-9.\n' | wc -c | tr -d ' ')
  echo "Number $n on row $r and column $c has $nsym symbols around it" \
    | tee -a /tmp/ac3.facts
  if test "$nsym" -ne 0
  then
    echo "Part $n on row $r and column $c" \
    | tee -a /tmp/ac3.facts
  fi
done

cp /tmp/ac3.facts /tmp/ac3.facts.old
cat /tmp/ac3.facts.old | awk '/Part/ { s += $2 }
  END { print "The sum of all part numbers is " s }' | tee -a /tmp/ac3.facts

rm /tmp/ac3.facts.old   #  One must keep flowing, like water.

echo "Yours Truly," | tee -a /tmp/ac3.facts
