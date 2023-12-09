#!/bin/sh

# Minify a Haskell file
#
# This performs dumb, textual replacements, as is not guaranteed to preserve the
# correctness of the code. Also, it is by no means exhaustive – it just serves
# as a starting point by doing some of the more obvious replacements.

test ! -f "$1" && echo "usage: $0 <file.hs>" && exit 1

out="${1%.*}.min.hs"

if test -f "$out"
then
    cp "$out" "out/$(basename $out).bak"
    wc -ml "$out"
else
    wc -ml "$1"
fi

cat $1 | \
  sed '/::/d;/^--/d' |
  > "$out"

wc -ml "$out"
