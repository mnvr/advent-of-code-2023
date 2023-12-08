import Data.Map qualified as M;import Data.Bifunctor;स=म"AAA";क=splitAt
main=interact$(++"\n").show.((,)<$>स<*>ग).प;झ 'L'=fst;झ _=snd;इ=fmap
ग भ@(_,ऋ)=foldl1 lcm$map(`म`भ)$ए ऋ;ए ऋ=filter((=='A').last)$M.keys ऋ
य _[_,_,'Z']_=[];य(व:ष)न ऋ=():य ष(झ व$ऋ M.!न)ऋ;त=क 3;ऋ=इ(द.drop 3).त
प=bimap head(M.fromList.map ऋ).क 2.lines;द=इ(fst.त.drop 2).त.tail
म न(ष,ऋ)=length$य(cycle ष)न ऋ
