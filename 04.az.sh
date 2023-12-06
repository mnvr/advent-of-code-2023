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
    wins[cardnum]=0;
    print "card", cardnum, "matches", g_matches[cardnum], "points", points

    for(i=1; i<=matches; i++) {
        j = cardnum+i
        print "> we win one extra instance of card", j, "which itself had", wins[j], "wins"
        wins[cardnum]+=(wins[j]+1);
    }
    print "card", cardnum, "wins", wins[cardnum]

  }

  END {
    print "---"
    totalCards=0;
    for(i=1;i<=nl;i++) {
        print "card", i, "extra wins", wins[i]
        totalCards+=1; # the card itself
        totalCards+=wins[i]; # and the extra cards it caused us to win
    }
    print "total scratchcards we are left with is", totalCards
  }
'
