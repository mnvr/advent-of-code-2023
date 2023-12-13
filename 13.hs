import Control.Applicative ((<|>), asum)
import Control.Arrow ((&&&))
import Data.List
import Data.Maybe (fromJust)
import Numeric
import Data.Bits

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

data Pattern = Pattern { rows :: [Int], cols :: [Int], ncol :: Int }

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pat pl : from (drop 1 rest)
    pat pl@(l:_) = pat' (length l) pl
    pat' n pl = Pattern { rows = ints pl, cols = ints (transpose pl), ncol = n }
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
  where ri Pattern { rows, cols } = (*100) <$> rIndex rows <|> rIndex cols

findAlternative :: Pattern -> Int
findAlternative Pattern { rows, cols, ncol } = fromJust rf
  where
    or = rIndex rows
    oc = rIndex cols
    rf = asum $ filter (/= or) $ map rIndex variants
    variants = [flip y x | y <- [0..length rows - 1], x <- [0..ncol - 1]]
    flip y x = zipWith (\i r -> if i == y then flipBit x r else r) [0..] rows
    flipBit x n = n `complementBit` x
