#!/bin/sh

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

nl=`awk 'END { print NR }' "$1"`
echo $nl

cat "$1" | tr -d '|' | nl | sort -nr | cut -f2- | awk -vnl=$nl '
  {
    p=0; s=0;
    for(i=3;i<=NF;i++) t[$i]+=1
    for(k in t) if(t[k]>1) p+=1
    if(p>0) s+=2^(p-1)
    delete t
    count=1
    # for(i=p;)
    memo[NR]=count
    ss += s
    print NR, nl-NR, p, s, ss
  }
  END { print ss; for(i=1;i<=nl;i++) print i, memo[i]; }
'
