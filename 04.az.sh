#!/bin/sh

cat examples/04 | tr -d '|' | awk '
  {
    p=0;
    for(i=3;i<=NF;i++) t[$i]+=1
    for(k in t) if(t[k]>1) p+=1
    if(p>0) s+=2^(p-1)
    delete t
  }
  END { print s }
'
