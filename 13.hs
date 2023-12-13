import Numeric
import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . parse

type Pattern = ([Int], [Int])

parse :: String -> [Pattern]
parse = from . lines
  where
    from [] = []
    from ls = let (pl, rest) = span (/= "") ls in pat pl : from (drop 1 rest)
    pat ls = (ints ls, ints (transpose ls))
    ints = map int
    int s = case readBin $ map (\c -> if c == '.' then '0' else '1') s of
        [(i, _)] -> i
