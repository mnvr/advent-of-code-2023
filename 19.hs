import Data.Char (isDigit)
import Data.List (elemIndex)
import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . parse

type Workflows = M.Map String [Rule]
type Rule = (Maybe Condition, String)
type Condition = (Int, Char, Int)
type Part = [Int]

parse :: String -> (Workflows, [Part])
parse = both . lines
  where
    both s = let (a, b) = span (/= "") s in (workflows a, map part b)
    workflows = M.fromList . map workflow
    workflow s = (rules . drop 1) <$> break (== '{') s
    rules [] = []
    rules s = let (a, b) = break (`elem` ",}") s in rule a : rules (drop 1 b)
    rule s = case break (== ':') s of
        (r, []) -> (Nothing, r)
        (c, r) -> (Just (condition c), r)
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
