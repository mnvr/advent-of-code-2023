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

p1 :: [Pattern] -> Int
p1 = sum . map (\p -> (mrow p) * 100 + (mcol p))

mcol :: Pattern -> Int
mcol p = go 1 (ncol p)
  where
    ncol (h:_) = length h
    go i n
      | i == n = 0
      | all (uncurry mirror) (map (\line -> splitAt i line) p) = i
      | otherwise = go (i + 1) n

mirror :: Eq a => [a] -> [a] -> Bool
mirror a b = let n = min (length a) (length b) in take n (reverse a) == take n b

mrow :: Pattern -> Int
mrow p = go 1 (length p)
  where
    go i n
      | i == n = 0
      | uncurry mirror (splitAt i p) = i
      | otherwise = go (i + 1) n
