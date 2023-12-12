import Data.List (intercalate)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse :: String -> [([String], [Int])]
parse = map line . lines
  where
    line l = case unfold (words l) of
        [s, num] -> (words $ map dot s, map read $ words $ map comma num)
    dot c = if c == '.' then ' ' else c
    comma c = if c == ',' then ' ' else c

-- Set this to 1 to toggle between p1 and p2
rc = 5 :: Int

unfold :: [String] -> [String]
unfold [s, xs] = [intercalate "?" (replicate rc s), intercalate "," (replicate rc xs)]

p1 :: [([String], [Int])] -> Int
p1 = sum . map count
  where count (ss, xs) = sum $ map (`consume` xs) (expand ss)

consume :: [String] -> [Int] -> Int
consume [] [] = 1
consume _  [] = 0
consume []  _ = 0
consume (s:ss) (x:xs) = if length s /= x then 0 else consume ss xs

expand :: [String] -> [[String]]
expand [] = [[]]
expand (s:ss)
  | '?' `notElem` s = map (s:) (expand ss)
  | otherwise = concatMap f (expand ss)
     where f ss = [words v ++ ss | v <- variations s]

variations :: String -> [String]
variations [] = [[]]
variations ('#':s) = map ('#':) (variations s)
variations ('?':s) = concatMap (\s -> ['#':s, ' ':s]) (variations s)
