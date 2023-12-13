import Numeric
import Data.List
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . parse

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
