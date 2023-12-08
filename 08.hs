import Data.Bifunctor (bimap, second)
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

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
