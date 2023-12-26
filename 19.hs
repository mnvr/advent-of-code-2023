import Data.Char (isDigit)
import Data.List (elemIndex, find)
import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

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
