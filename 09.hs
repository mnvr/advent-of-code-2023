import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

parse :: String -> [[Int]]
parse = map (map read . words) . lines

p1 :: [[Int]] -> Int
p1 = sum . map (foldr (\ds d -> last ds + d) 0 . dxs)

p2 :: [[Int]] -> Int
p2 = p1 . map reverse

dxs :: [Int] -> [[Int]]
dxs xs = if all (== 0) xs then [xs] else xs : dxs dx
  where dx = zipWith (-) (drop 1 $ xs) xs
