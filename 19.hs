import Data.Char (isDigit)

main :: IO ()
main = interact $ (++ "\n") . show . parse

parse = both . lines
  where
    both ("":ls) = ([], map part ls)
    both (l:ls) = let (w, p) = both ls in (workflow l : w, p)
    workflow s = "workflow " ++ s
    part :: String -> [Int]
    part s = case reads (snd $ break isDigit s) of
        [] -> []
        [(d, r)] -> d : part r
