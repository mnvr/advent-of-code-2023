import Control.Applicative ((<|>), asum)
import Control.Arrow ((&&&))
import Data.List
import Data.Maybe (fromJust)
import Numeric
import Data.Bits

main :: IO ()
-- main = interact $ (++ "\n") . show . p1 . parse
main = interact $ (++ "\n") . show . p2 . parse

type Pattern = ([String], [String])

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pat pl : from (drop 1 rest)
    pat = ints &&& ints . transpose
    ints = map id
    -- int s = case readBin $ map (\c -> if c == '.' then '0' else '1') s of
        -- [(i, _)] -> i

rIndex :: [String] -> Maybe Int
rIndex xs = find f [1..length xs - 1]
  where f i = let (a, b) = splitAt i xs
                  j = min (length a) (length b)
              in take j (reverse a) == take j b

p1 :: [Pattern] -> Int
p1 = sum . map (fromJust . ri)
  where ri (rows, cols) = (*100) <$> rIndex rows

-- p2 :: [Pattern] -> Int
p2 = map findAlternative

-- findAlternative :: Pattern -> Int
findAlternative (rows, cols) = map rIndex rowVariants -- debug $ (*100) <$> rf <|> rc
  where
    or = rIndex rows
    oc = rIndex cols
    rf = asum $ filter (const True) $ map rIndex rowVariants
    rc = asum $ filter (const True) $ map rIndex colVariants
    rowVariants = [flip y x rows | y <- [0..length rows - 1], x <- [0..length cols - 1]]
    colVariants = [flip x y cols | y <- [0..length rows - 1], x <- [0..length cols - 1]]
    flip y x ns = zipWith (\i r -> if i == y then flipBit x r else r) [0..] ns
    -- flipBit x n = n `complementBit` x
    flipBit x s = let (a, b:c) = splitAt x s in a ++ [flipC b] ++ c
    flipC '.' = '#'
    flipC '#' = '.'

    debug (Just v) = v
    debug (Nothing) = error (show (rows, cols))
