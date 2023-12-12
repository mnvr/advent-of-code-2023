import Data.Map qualified as M
import Data.List (intercalate)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1 :: [(String, [Int])] -> Int
p1 inp = sum (map count inp)
  where
    count (s, xs) = length $ filter (== xs) $ map runs $ arrangements s
    runs :: String -> [Int]
    runs [] = []
    runs s = case takeWhile (== '#') s of
        [] -> runs (drop 1 s)
        prefix -> let len = length prefix in len : runs (drop len s)
    matchingRuns :: [Int] -> [[Int]] -> Int
    matchingRuns xs = length . filter (== xs)

arrangements :: String -> [String]
arrangements s = snd $ arr M.empty s

type MemoTable = M.Map String [String]

arr :: MemoTable -> String -> (MemoTable, [String])
arr m [] = (m, [])
arr m "?" = (m, [".", "#"])
arr m [c] = (m, [[c]])
arr m s = case M.lookup s m of
      Nothing -> arrDo m s
      Just r -> (m, r)
    where
        arrDo :: MemoTable -> String -> (MemoTable, [String])
        arrDo m (c:cs) =
            let (m', r) = arr m cs
                r' = if c == '?' then concatMap (\a -> ['.': a, '#':a]) r else map (\a -> c: a) r
            in (M.insert s r' m, r')

-- p2 :: [(String, [Int])] -> Int
p2 = p1 . unfold
-- p2 = unfold

unfold :: [(String, [Int])] -> [(String, [Int])]
unfold = map f
  where f (s, xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))
