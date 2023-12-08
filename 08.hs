import Data.Bifunctor (bimap, second)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Debug.Trace (trace)

main :: IO ()
-- main = interact $ (++ "\n") . show . (p1 &&& p2) . parse
main = interact $ (++ "\n") . show . p2 . parse

type Network = [(String, (String, String))]

parse :: String -> (String, Network)
parse = bimap head (map network) . splitAt 2. lines
  where network = second (pair . drop 3) . label
        pair = second (fst . label . drop 2) . label . tail
        label = splitAt 3

p1 :: (String, Network) -> Int
p1 (instructions, network) = next instructions "AAA" 0
  where next [] node c = if node == "ZZZ" then c else next instructions node c
        next (i:is) node c = next is (move i $ fromJust $ lookup node network) (c + 1)

move :: Char -> (String, String) -> String
move i = if i == 'L' then fst else snd

p2 :: (String, Network) -> Int
p2 (instructions, network) = next instructions startNodes 0
  where next :: String -> [String] -> Int -> Int
        next [] nodes c = if all isEnd nodes then c else next instructions nodes c
        next (i:is) nodes c = trace ("next " ++ [i] ++ ":" ++ show (length is) ++ " " ++ show (length nodes) ++ " " ++ show c ++ "     " ++ show nodes) $ if all isEnd nodes then c else next is (map next' nodes) (c + 1)
          where next' node = move i $ fromJust $ lookup node network
        startNodes = filter isStart $ map fst network
        isStart = (== 'A') . last
        isEnd = (== 'Z') . last
