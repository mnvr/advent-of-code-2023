main=interact$(++"\n").show.((,)<$>a<*>a.map reverse).map(map read.words).lines
a=sum.map(sum.map last.w);w=takeWhile(any(/=0)).iterate d
d z=zipWith(-)(drop 1$z)z
