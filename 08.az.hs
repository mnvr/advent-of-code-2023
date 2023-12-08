import Data.Bifunctor;import Control.Arrow((&&&))
import Data.Map qualified as M;य _[_,_,'Z']_=[];य(व:ष)न ऋ=():य ष(झ व$ऋ M.!न)ऋ
main=interact$(++"\n").show.(स&&&ग).प;प=bimap head(M.fromList.map ऋ).क 2.lines
ऋ=इ(द.drop 3).त;द=इ(fst.त.drop 2).त.tail;त=क 3;स=म"AAA";इ=second;क=splitAt;
ग भ@(_,ऋ)=foldl1 lcm$map(`म`भ)$ए ऋ;ए ऋ=filter((=='A').last)$M.keys ऋ
म न(ष,ऋ)=length$य(cycle ष)न ऋ;झ 'L'=fst;झ _=snd
