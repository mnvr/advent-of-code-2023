import Data.Char;import Data.List
main=interact$(++"\n").show.sum.map p.lines
p s=f s id*10+f(reverse s)reverse
f s r=if(isDigit.head)s then read [head s] else maybe (f(tail s)r) (+1)$findIndex(`isPrefixOf`s)$map r ["one","two","three","four","five","six","seven","eight","nine"]
