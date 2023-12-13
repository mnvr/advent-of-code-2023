main :: IO ()
main = interact $ (++ "\n") . show . p1 . patterns

type Pattern = [String]

patterns :: String -> [Pattern]
patterns = reverse .  last . foldl f ([], []) . lines
  where
    f (p, ps) [] = ([], reverse p : ps)
    f (p, ps) line = (line:p, ps)
    last (p, ps) = if null p then ps else reverse p : ps

p1 = id
