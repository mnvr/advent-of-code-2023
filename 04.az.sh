#!/bin/sh

# 2**(len(line.split()) - len(set(line.split())) - 1)
cat examples/04 | head -1 | tr -d '|' | awk '{ for(i=3;i<=NF;i++) print $i+0; }'
