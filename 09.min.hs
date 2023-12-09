main=interact$(++"\n").show.((,)<$>a<*>a.map reverse).map(map read.words).lines
w z=if all(==0)z then[z]else z:w(zipWith(-)(drop 1$z)z)
a=sum.map(foldr(\u d->last u+d)0.w)
