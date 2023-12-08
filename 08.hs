import Data.Bifunctor (bimap, second)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Instructions = String
type Node = String
type Network = M.Map Node (Node, Node)

parse :: String -> (Instructions, Network)
parse = bimap head (M.fromList . map network) . splitAt 2. lines
  where network = second (pair . drop 3) . label
        pair = second (fst . label . drop 2) . label . tail
        label = splitAt 3

p1 :: (Instructions, Network) -> Int
-- Note: The third example does not have an "AAA"
p1 = pathLength "AAA"

pathLength :: Node -> (Instructions, Network) -> Int
pathLength node (is, network) = length $ path (cycle is) node
  where path _ [_, _, 'Z'] = []
        path (i:is) node = () : path is (move i $ fromJust $ M.lookup node network)

move :: Char -> (Node, Node) -> Node
move i = if i == 'L' then fst else snd

p2 :: (Instructions, Network) -> Int
p2 input@(_, network) = foldl1 lcm $ map (`pathLength` input) startNodes
  where startNodes = filter ((== 'A') . last) $ M.keys network
