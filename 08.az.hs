import Data.Bifunctor
import Control.Arrow ((&&&))
import Data.Map qualified as M

main=interact$(++"\n").show.(स&&&ग).प

प=bimap head(M.fromList.map ऋ).splitAt 2.lines
ऋ=second(द.drop 3).त
द=second(fst.त.drop 2).त.tail
त=splitAt 3
स=म"AAA"

म न(ष,ऋ)=length$य(cycle ष)न ऋ
य _[_,_,'Z']_=[]
य(व:ष)न ऋ=():य ष(झ व$ऋ M.!न)ऋ
झ 'L'=fst
झ _=snd

ग भ@(_,ऋ)=foldl1 lcm$map(`म`भ)$ए ऋ
ए ऋ=filter((=='A').last)$M.keys ऋ
