import Control.Applicative ((<|>), asum)
import Control.Arrow ((&&&))
import Data.List
import Data.Maybe (fromJust)
import Numeric
import Data.Bits

-- A variant of 13.hs that directly uses the string representation instead of
-- first converting them to their bitwise int representations. Somewhat
-- surprisingly, this is not slower. There is a slight difference when
-- optimized: under -O2 the bitwise representation version is slightly faster.

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Pattern = ([String], [String])

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pat pl : from (drop 1 rest)
    pat = id &&& transpose

rIndex :: [String] -> Maybe Int
rIndex = asum . rIndices

rIndices :: [String] -> [Maybe Int]
rIndices xs = map f [1..length xs - 1]
  where f i = let (a, b) = splitAt i xs
                  j = min (length a) (length b)
              in if take j (reverse a) == take j b then Just i else Nothing

p1 :: [Pattern] -> Int
p1 = sum . map (fromJust . ri)
  where ri (rows, cols) = (*100) <$> rIndex rows <|> rIndex cols

p2 :: [Pattern] -> Int
p2 = sum . map smudge

smudge :: Pattern -> Int
smudge (rows, cols) = fromJust $ (*100) <$> rf <|> rc
  where
    or = rIndex rows
    oc = rIndex cols
    rf = asum $ filter (/= or) $ concatMap rIndices rowVariants
    rc = asum $ filter (/= oc) $ concatMap rIndices colVariants
    rowVariants = [flip y x rows | y <- [0..length rows - 1], x <- [0..length cols - 1]]
    colVariants = [flip x y cols | y <- [0..length rows - 1], x <- [0..length cols - 1]]
    flip y x ns = zipWith (\i r -> if i == y then flipBit x r else r) [0..] ns
    flipBit x s = let (a, b:c) = splitAt x s in a ++ [flipC b] ++ c
    flipC '.' = '#'
    flipC '#' = '.'
