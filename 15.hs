import Data.Char (ord)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . steps . lines

steps :: [String] -> [String]
steps = concatMap f
  where
    f [] = []
    f s = let (a, b) = span (/= ',') s in a : f (drop 1 b)

p1 :: [String] -> Int
p1 = sum . map hash

hash :: String -> Int
hash = foldl (\v c -> (v + (ord c)) * 17 `mod` 256) 0
