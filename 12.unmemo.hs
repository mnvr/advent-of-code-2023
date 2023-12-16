import Data.List (nub, intercalate)

-- The un-memoized, original formulation of 12.hs. Is only good enough for the
-- examples, not the full input.

main :: IO ()
main = interact $ (++ "\n") . show . p2 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1, p2 :: [(String, [Int])] -> Int
p1 = sum . map (uncurry ways)
p2 = p1 . unfold

unfold :: [(String, [Int])] -> [(String, [Int])]
unfold = map f
  where f (s, xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))

ways :: String -> [Int] -> Int
ways [] [] = 1
ways [] [x] = 0
ways s [] = if none '#' s then 1 else 0
ways ('.':rs) xs = ways rs xs
ways ('?':rs) xs = ways rs xs + ways ('#':rs) xs
ways s (x:rx) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = ways (drop (x + 1) s) rx
ways _ _ = 0

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Char -> String -> Bool
only c = all (== c) . nub

none :: Char -> String -> Bool
none c = not . any (== c) . nub
