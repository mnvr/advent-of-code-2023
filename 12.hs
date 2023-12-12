{-# OPTIONS_GHC -Wno-all #-}

import Data.Map qualified as M
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . p1 . head . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1' :: [(String, [Int])] -> Int
p1' inp = sum (map count inp)
  where
    count (s, xs) = length $ filter (== xs) $ map runs $ arrangements s
    runs :: String -> [Int]
    runs [] = []
    runs s = case takeWhile (== '#') s of
        [] -> runs (drop 1 s)
        prefix -> let len = length prefix in len : runs (drop len s)
    matchingRuns :: [Int] -> [[Int]] -> Int
    matchingRuns xs = length . filter (== xs)

-- p1 :: [(String, [Int])] -> Int
-- p1 inp = runs' (fst (head inp)) -- sum (map count inp)
p1 (s, xs) = consume s xs
  where
    count (s, xs) = length $ filter (== xs) $ runs' s

consume :: String -> [Int] -> Int
consume s [] = trace ("base case for [] []") 1
consume s [] = trace ("base case for s  []: " ++ s) $ if nub s == "." then 1 else 0
consume s (x:xs) = trace ("normal case for x " ++ show x ++ "  " ++ s) $
    if take (x + 1) s == (x `replicate` '#') ++ "." then trace ("case a") $ consume (drop (x + 1) s) xs
    else if take (x + 1) s == (x `replicate` '#') ++ "?" then trace ("case b") $ consume (drop (x + 1) s) xs
    else if take (x + 1) s == (x `replicate` '#') ++ [] then trace ("case c") $ consume (drop (x + 1) s) xs
    else if take 1 s == "." then trace ("case d") $ consume (dropWhile (== '.') s) (x:xs)
    else if take 1 s == "?" && x == 1 then trace ("case e") $ (
        let a = consume (drop 1 s) xs           -- taking ? as #
            b = consume (drop 1 s) (x:xs)       -- taking ? as .
        in trace ("case e got a " ++ show a ++ " b " ++ show b) $ if a == 0 then b else if b == 0 then a else a + b)
    else if take 1 s == "?" then trace ("case f") $ (
        let a = consume (drop 1 s) ((x-1) : xs) -- taking ? as #
            b = consume (drop 1 s) (x:xs)       -- taking ? as .
        in trace ("case f got a " ++ show a ++ " b " ++ show b) $ if a == 0 then b else if b == 0 then a else a + b)
    -- else if (nub (take x s)) \\ "?#" == "" && take 1 (drop x s) /= "#" then consume (drop (x + 1) s) xs
    -- else if (nub (take x s)) \\ "?#" == "" && take 1 (drop x s) == "?" then consume (drop (x + 1) s) xs + consume (drop 1 s) (x:xs)
    -- else if take 1 s == "#" then 0
    else 0
    -- else if take (x + 1) s == (x `replicate` '?') ++ '.' then consume (drop (x + 1) s) xs
    -- else if take (x + 1) s == (x `replicate` '?') ++ '.' then consume (drop (x + 1) s) xs

--only ::
-- hasPrefixLen :: Char -> Int -> String -> Bool
-- hasPrefixLen c i s = i `replicate` c `isPrefixOf` s

runs' :: String -> [[Int]]
runs' [] = [[]]
runs' ('.':cs) = runs' cs
runs' ('#':[]) = [[1]]
runs' ('#':'.':cs) = map (1:) (runs' cs)
runs' ('#':'#':cs) = map (\(rh:rs) -> (rh+1):rs) (runs' ('#':cs))
runs' ('?':'#':cs) = concatMap (\(rh:rs) -> [rh:rs, (rh+1):rs]) (runs' ('#':cs))
-- runs' ('?':'?':cs) = [map (1:) (runs' ('.':cs))]
-- runs' ('?':'.':cs) = map (1:) (runs' ('.':cs))
-- runs' ('?':'#':cs) = runs' ('.':cs))

-- runs' s@('#':cs) = let (prefix, rest) = span (== '#') s
--                        r = length prefix
--                    in map (r:) (runs' rest)
-- -- runs' ('?':cs) = concatMap (\(rx:rs) -> [rx:rs, (rx+1):rs]) (runs' cs)
-- runs' ('?':cs) = concatMap f (runs' cs)
--   where
--     f (rx:rs) = if length (takeWhile (== '#') cs) == rx then [rx:rs, 1+rx:rs]

satisfy :: [Char] -> [Int] -> Bool
satisfy _ [] = True
satisfy s [x] = length (takeWhile (== '#') (dropWhile (/= '#') s)) == x


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
-- p2 = p1 . unfold

unfold :: [(String, [Int])] -> [(String, [Int])]
unfold = map f
  where f (s, xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))
