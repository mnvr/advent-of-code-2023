#!/bin/sh

# WIP
# Spam facts about the problem, until we have enough facts to solve it.
#
# The Allez Cuisine theme for today was spam. This solution writes a very, very,
# long email, putting in all sorts of content just to spam the keyword matchers.
# But interestingly, when doing this, it manages to also find the solution.

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1
head -1 "$1" > /dev/null || exit 1

echo "Hello Dear May," | tee /tmp/ac3.facts

read_log () { cp /tmp/ac3.facts /tmp/ac3.facts.old; cat /tmp/ac3.facts.old; }
nested_read_log () { cat /tmp/ac3.facts.old; }
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
          print "Number", n, "on row", NR, "and column", start
          n = ""
          start = 0
        }
        if (c != ".") {
          print "Symbol", c, "on row", NR, "and column", i
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

read_log | awk '/Symbol */ { print $5, $8 }' |
while read r c
do
  echo '> Symbol *' "on row $r and column $c" | log
  echo grep -E "on row (`expr $r - 1`|$r|`expr $r + 1`) "
  nested_read_log | grep -E "on row (`expr $r - 1`|$r|`expr $r + 1`) " |
  awk -vr=$r -vc=$c '/Part */ {
    n = $2; ns = $8; ne = ns + length(n) - 1;
    print ">>", $0, "spans from", ns, "to", ne
    print ">>", "To touch", c, "should be between", (ns - 1), "and", (ne + 1)
    if (c >= (ns - 1) && c <= (ne + 1)) {
      print "Gear on row", r "and column", c "touches part", n
    }
  }' | log
done

read_log | awk '/touches part/ { print $4, $7 }' | sort | uniq -c |
awk '
  $1 == 2 { print "Gear on row", $2, "and column", $3, "touches two parts" }
' | log

# while read r c n
# do
#done

read_log | awk '/Part/ { s += $2 }
  END { print "The sum of all part numbers is", s }' | log

rm /tmp/ac3.facts.old   #  One must keep flowing, like water.

echo "Yours Truly," | log
