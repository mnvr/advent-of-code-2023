#!/bin/sh

# 2**(len(line.split()) - len(set(line.split())) - 1)
cat examples/04 | head -1 | tr -d '|' | awk '{ for(i=3;i<=NF;i++) print cs[$i]+=1; } END { for(c in cs) print c; for(c in cs) if (cs[c]>1) {s+=1; print cs[c], s;}; print (2 ^ (s - 1)) }'
