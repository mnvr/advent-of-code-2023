main=interact$(++"\n").show.((,)<$>a<*>b).parse
parse=map(map read.words).lines
a=sum.map(foldr(\u d->last u+d)0.w)
b=a.map reverse
w z=if all(==0)z then[z]else z:w(zipWith(-)(drop 1$z)z)
