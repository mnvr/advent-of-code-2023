import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

parse :: String -> [[Int]]
parse = map (map read . words) . lines

p1 :: [[Int]] -> Int
p1 = sum . map (sum . map last . dxs)

p2 :: [[Int]] -> Int
p2 = p1 . map reverse

dxs :: [Int] -> [[Int]]
dxs = takeWhile (any (/= 0)) . iterate dx
  where dx xs = zipWith (-) (drop 1 $ xs) xs
