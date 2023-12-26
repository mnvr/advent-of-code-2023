import Data.Char (isDigit)
import Data.List (elemIndex)

main :: IO ()
main = interact $ (++ "\n") . show . parse

type Workflow = (String, [Rule])
type Rule = (Maybe Condition, String)
type Condition = (Int, Char, Int)
type Part = [Int]

parse :: String -> ([Workflow], [Part])
parse = both . lines
  where
    both ("":ls) = ([], map part ls)
    both (l:ls) = let (w, p) = both ls in (workflow l : w, p)
    workflow s = (rules . drop 1) <$> break (== '{') s
    rules [] = []
    rules s = let (a, b) = break (`elem` ",}") s in rule a : rules (drop 1 b)
    rule s = case break (== ':') s of
        (r, []) -> (Nothing, r)
        (c, r) -> (Just (condition c), r)
    condition (p:c:rs) = case p `elemIndex` "xmas" of
        Just i -> (i, c, read rs)
    part :: String -> [Int]
    part s = case reads (snd $ break isDigit s) of
        [] -> []
        [(d, r)] -> d : part r
