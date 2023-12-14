import Data.List (nub, intercalate)
import Data.Map qualified as M

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
ways s xs = snd $ memo M.empty s xs
  where
    memo :: M.Map (String, [Int]) Int -> String -> [Int] -> (M.Map (String, [Int]) Int, Int)
    memo m s xs = let key = (s, xs) in case M.lookup key m of
      Just v -> (m, v)
      Nothing -> let (m', v) = ways' memo m s xs
                 in (M.insert key v m', v)

type MT = M.Map (String, [Int]) Int

ways' :: (MT -> String -> [Int] -> (MT, Int)) -> MT -> String -> [Int] -> (MT, Int)
ways' f m [] [] = (m, 1)
ways' f m [] [x] = (m, 0)
ways' f m s [] = (m, if none '#' s then 1 else 0)
ways' f m ('.':rs) xs = f m rs xs
ways' f m ('?':rs) xs = let (m', a) = f m rs xs
                            (m'', b) = f m' ('#':rs) xs
                        in (m'', a + b)
ways' f m s (x:rx) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = f m (drop (x + 1) s) rx
ways' _ m _ _= (m, 0)

after :: Int -> Char -> String -> Bool
after x c s = only c (take 1 (drop x s))

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Char -> String -> Bool
only c = all (== c) . nub

none :: Char -> String -> Bool
none c = not . any (== c) . nub
