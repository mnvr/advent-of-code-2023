import Data.Char;import Data.List
main=interact$(++"\n").show.sum.map p.lines
p s=f s id*10+(($ s)>>=f)reverse
f s@(h:t) r=if isDigit h then read [h] else maybe (f t r) (+1)$findIndex(`isPrefixOf`s)$map r ["one","two","three","four","five","six","seven","eight","nine"]
