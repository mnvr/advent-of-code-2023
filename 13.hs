import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . p1 . patterns

type Pattern = [String]

patterns :: String -> [Pattern]
patterns = reverse .  last . foldl f ([], []) . lines
  where
    f (p, ps) [] = ([], reverse p : ps)
    f (p, ps) line = (line:p, ps)
    last (p, ps) = if null p then ps else reverse p : ps

p1 = map rcol

rcol :: Pattern -> Maybe Int
rcol p = go 1 (ncol p)
  where
    ncol (h:_) = length h
    go i n
      | i == n = Nothing
      | all (uncurry mirror) (map (\line -> splitAt i line) p) = Just i
      | otherwise = go (i + 1) n
    mirror l r = let n = min (length l) (length r) in take n (reverse l) == take n r
