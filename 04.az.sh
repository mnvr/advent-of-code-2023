#!/bin/sh

test -z "$1" && echo "usage: $0 <path-to-input>" && exit 1

nl=`awk 'END { print NR }' "$1"`
echo "total cards $nl"

cat "$1" | tr -d '|' | nl | sort -nr | cut -f2- | awk -vnl=$nl '
  {
    p=0; s=0; matches=0; points=0;
    for(i=3;i<=NF;i++) t[$i]+=1
    for(k in t) if(t[k]>1) {p+=1; matches+=1;}
    if(p>0) s+=2^(p-1)
    ss += s;
    delete t

    if (matches>0) { points=2^(matches-1)};


    cardnum=nl-NR+1
    instances[cardnum]=1;
    g_matches[cardnum]=matches;
    print "card", cardnum, "matches", g_matches[cardnum], "points", points, "instances", instances[cardnum]

    for(i=1; i<=matches; i++) {
        j = cardnum+1
        print "> we win one extra instance of card", j;
        instances[j]+=1;
        print "> card", j, "itself had", g_matches[j], "matches"
        # for(k=g_matches)
    }
  }

  END {
    print "---"
    for(i=1;i<=nl;i++) {
        print "card", i, "instances", instances[i]
    }
  }
'
