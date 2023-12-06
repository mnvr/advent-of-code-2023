#!/bin/sh

cat $1 | tr -d '|' | nl | sort -nr | cut -f2- |
awk -vn=`awk 'END{print NR}' $1` '
  {
    m=p=0
    for(i=3;i<=NF;i++) c[$i]+=1
    for(k in c) if(c[k]>1) m+=1
    delete c
    if(m>0)p=2^(m-1)
    s+=p
    i=n-NR+1
    for(j=1;j<=m;j++)w[i]+=(w[i+j]+1)
  } END {
    for(i=1;i<=n;i++)t+=(w[i]+1)
    print s "," t
  }
'
