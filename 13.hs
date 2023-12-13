import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.List
import Data.Maybe (fromJust)
import Numeric

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Pattern = ([Int], [Int])

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pat pl : from (drop 1 rest)
    pat = ints &&& ints . transpose
    ints = map int
    int s = case readBin $ map (\c -> if c == '.' then '0' else '1') s of
        [(i, _)] -> i

rIndex :: [Int] -> Maybe Int
rIndex xs = find f [1..length xs - 1]
  where f i = let (a, b) = splitAt i xs
                  j = min (length a) (length b)
              in take j (reverse a) == take j b

p1 :: [Pattern] -> Int
p1 = sum . map (fromJust . ri)
  where ri (rows, cols) = (*100) <$> rIndex rows <|> rIndex cols
