import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.List (transpose, find)
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Pattern = [String]

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pl : from (drop 1 rest)

-- For all splits, find the differences across the reflection line. For part 1,
-- where the mirroring is perfect, the won't be any difference. For part 2,
-- where there's a single smidge, there'll be exactly one difference.

reflectionLine :: Int -> [String] -> Maybe Int
reflectionLine dx xs = find f [1..length xs - 1]
  where f i = let (a, b) = splitAt i xs in difference (reverse a) b == dx

difference :: [String] -> [String] -> Int
difference xs = sum . zipWith rd xs
  where rd r = sum . zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) r

solve :: Int -> [Pattern] -> Int
solve dx = sum . map (fromJust . f)
 where f p = (*100) <$> reflectionLine dx p <|> reflectionLine dx (transpose p)

p1, p2 :: [Pattern] -> Int
p1 = solve 0
p2 = solve 1
