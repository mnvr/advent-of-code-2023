import Data.Char (isDigit)
import Data.List (elemIndex, find)
import Data.Map qualified as M
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Workflows = M.Map String [Rule]
type Rule = (Maybe Condition, String)
type Condition = (Int, Char, Int)
type Part = [Int]

parse :: String -> (Workflows, [Part])
parse = both . lines
  where
    both s = let (a, b) = span (/= "") s in (workflows a, map part (drop 1 b))
    workflows = M.fromList . map workflow
    workflow s = (rules . drop 1) <$> break (== '{') s
    rules [] = []
    rules s = let (a, b) = break (`elem` ",}") s in rule a : rules (drop 1 b)
    rule s = case break (== ':') s of
        (r, []) -> (Nothing, r)
        (c, r) -> (Just (condition c), drop 1 r)
    condition (p:c:rs) = case p `elemIndex` "xmas" of
        Just i -> (i, c, read rs)
    part s = case reads (snd $ break isDigit s) of
        [] -> []
        [(d, r)] -> d : part r

valid :: Workflows -> Part -> Bool
valid ws p = go "in"
  where
    go "A" = True
    go "R" = False
    go w = go $ next ((M.!) ws w)
    next ((Nothing, r) : _) = r
    next ((Just c, r) : rs) = if test c then r else next rs
    test (i, '<', v) = (p !! i) < v
    test (i, '>', v) = (p !! i) > v

p1 :: (Workflows, [Part]) -> Int
p1 (workflows, parts) = sum . concat $ filter (valid workflows) parts

type Ranges = [(Int, Int)]
type Thread = (Ranges, String)

validCombinations :: Workflows -> Int
validCombinations ws = go [(replicate 4 (1, 4000), "in")]
  where
    combo :: Ranges -> Int
    combo ranges = product $ map ((+1) . uncurry subtract) ranges
    rules w = ((M.!) ws w)
    go :: [Thread] -> Int
    go [] = 0
    go ((rs, "A") : xs) = combo rs + go xs
    go ((_, "R") : xs) = go xs
    go ((rs, w) : xs) = go $ (splitThreads rs (rules w)) ++ xs
    splitThreads :: Ranges -> [Rule] -> [Thread]
    splitThreads rs ((Nothing, w) : _) = [(rs, w)]
    splitThreads rs ((Just c, w) : rest) =
        let (matching, notMatching) = split rs c
        in [(matching, w)] ++ splitThreads notMatching rest
    split :: Ranges -> Condition -> (Ranges, Ranges)
    split ranges (i, op, v) = foldl f ([], []) (zip [0..] ranges)
      where f (m, n) (j, r) | i == j = let (match, nomatch) = split' r op v
                                       in (m ++ [match], n ++ [nomatch])
                            | otherwise = (m ++ [r], n ++ [r])
    split' (a, b) '<' v = ((a, v - 1), (v, b))
    split' (a, b) '>' v = ((v + 1, b), (a, v))

p2 :: (Workflows, [Part]) -> Int
p2 (workflows, _) = validCombinations workflows
