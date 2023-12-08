import Data.Bifunctor
import Control.Arrow ((&&&))
import Data.Map qualified as M

main = interact $ (++ "\n") . show . (स &&& ग) . प

प = bimap head (M.fromList . map ऋ) . splitAt 2. lines
  where ऋ = second (द . drop 3) . त
        द = second (fst . त . drop 2) . त . tail
        त = splitAt 3

स = म "AAA"

म न (ष, ऋ) = length $ य (cycle ष) न
  where य _ [_, _, 'Z'] = []
        य (i:ष) न = () : य ष ((if i == 'L' then fst else snd) $ ऋ M.! न)

ग input@(_, ऋ) = foldl1 lcm $ map (`म` input) ए
  where ए = filter ((== 'A') . last) $ M.keys ऋ
