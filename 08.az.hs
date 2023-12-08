import Data.Bifunctor;import Control.Arrow((&&&))
import Data.Map qualified as M;प=bimap head(M.fromList.map ऋ).क 2.lines
main=interact$(++"\n").show.(स&&&ग).प;ए ऋ=filter((=='A').last)$M.keys ऋ
ऋ=इ(द.drop 3).त;य _[_,_,'Z']_=[];य(व:ष)न ऋ=():य ष(झ व$ऋ M.!न)ऋ;क=splitAt
म न(ष,ऋ)=length$य(cycle ष)न ऋ;द=इ(fst.त.drop 2).त.tail;स=म"AAA";त=क 3
ग भ@(_,ऋ)=foldl1 lcm$map(`म`भ)$ए ऋ;झ 'L'=fst;झ _=snd;इ=second
