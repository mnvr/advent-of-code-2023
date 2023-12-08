import Data.Bifunctor (bimap, second)
import Control.Arrow ((&&&))
import Data.Map qualified as M

main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

parse = bimap head (M.fromList . map network) . splitAt 2. lines
  where network = second (pair . drop 3) . label
        pair = second (fst . label . drop 2) . label . tail
        label = splitAt 3

p1 = pathLength "AAA"

pathLength node (is, network) = length $ path (cycle is) node
  where path _ [_, _, 'Z'] = []
        path (i:is) node = () : path is ((if i == 'L' then fst else snd) $ network M.! node)

p2 input@(_, network) = foldl1 lcm $ map (`pathLength` input) startNodes
  where startNodes = filter ((== 'A') . last) $ M.keys network
