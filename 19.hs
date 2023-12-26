import Data.Char (isDigit)

main :: IO ()
main = interact $ (++ "\n") . show . parse

type Workflow = (String, [Rule])
type Rule = (Maybe Condition, String)
type Condition = String -- (Int, Char, Int)
type Part = [Int]

parse = both . lines
  where
    both ("":ls) = ([], map part ls)
    both (l:ls) = let (w, p) = both ls in (workflow l : w, p)
    workflow s = (rules . drop 1) <$> break (== '{') s
    rules [] = []
    rules s = let (a, b) = break (`elem` ",}") s in a : rules (drop 1 b)
    part :: String -> [Int]
    part s = case reads (snd $ break isDigit s) of
        [] -> []
        [(d, r)] -> d : part r
