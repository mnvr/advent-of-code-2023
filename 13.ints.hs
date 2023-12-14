import Control.Applicative ((<|>), asum)
import Control.Arrow ((&&&))
import Data.Bits (complementBit)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Numeric (readBin)

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

-- A variant of 13.hs that uses more explicit reflection testing. Additionally,
-- it also uses an integer representation, although that doesn't make too big of
-- a difference in runtime (compared to this same code, but using the strings
-- themselves for various checks below).

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
rIndex = asum . rIndices

rIndices :: [Int] -> [Maybe Int]
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
    flipBit x n = n `complementBit` x
