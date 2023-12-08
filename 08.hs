import Data.Bifunctor (bimap, second)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Instructions = String
type Node = String
type Network = [(Node, (Node, Node))]

parse :: String -> (Instructions, Network)
parse = bimap head (map network) . splitAt 2. lines
  where network = second (pair . drop 3) . label
        pair = second (fst . label . drop 2) . label . tail
        label = splitAt 3

p1 :: (Instructions, Network) -> Int
-- Note: The third example does not have an "AAA"
p1 = pathLength "AAA"

pathLength :: Node -> (Instructions, Network) -> Int
pathLength node (instructions, network) = next instructions node 0
  where next [] node c = if isEnd node then c else next instructions node c
        next (i:is) node c = next is m (c + 1)
            where m = move i $ fromJust $ lookup node network

move :: Char -> (Node, Node) -> Node
move i = if i == 'L' then fst else snd

isStart, isEnd :: Node -> Bool
isStart = (== 'A') . last
isEnd = (== 'Z') . last

p2 :: (Instructions, Network) -> Int
p2 input@(_, network) = foldl1 lcm $ map (`pathLength` input) startNodes
  where startNodes = filter isStart $ map fst network
