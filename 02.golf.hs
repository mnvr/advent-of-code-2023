import Data.Text qualified as T
main=interact$(++"\n").show.(\g->(s g,t g)).map j.lines
e c=T.split(==c)
j s=(c,r)where[a,b]=e ':'$T.pack s;c=read.T.unpack.last$T.words a;r=map w$e ';'b
w t=foldl u z$e ','t
u(r,g,b)x=case T.unpack d of"red"->(c,g,b);"green"->(r,c,b);"blue"->(r,g,c)
 where[a,d]=T.words x;c=(read.T.unpack)a
z=(0,0,0)
v(r,g,b)=r<=12&&g<=13&&b<=14
s=sum.map fst.filter(all v.snd)
p(r,g,b)=r*g*b
f d g=foldl m d(snd g)where m (r,g,b) (u,v,w)=(max r u,max g v,max b w)
t=sum.map(p.f z)
