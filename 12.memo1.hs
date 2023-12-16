import Data.List (nub, intercalate)
import Data.Map qualified as M

-- The manually memoized (without using the State monad) variation of 12.hs.
--
-- This doesn't actually do any memoization, it is a stepping stone to the real
-- version. This is a part of a demonstration showing how we go from an
-- unmemoized version (12.unmemo.hs) to a memoized version using the (State)
-- monad (12.hs).

main :: IO ()
main = interact $ (++ "\n") . show . p2 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1, p2 :: [(String, [Int])] -> Int
p1 = sum . map ways
p2 = p1 . unfold

unfold :: [(String, [Int])] -> [(String, [Int])]
unfold = map f
  where f (s, xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))

type Rx = (String, [Int])
type Memo = M.Map Rx Int

ways :: Rx -> Int
ways = ways' memo M.empty
  where memo m k = ways' memo m k

ways' :: (Memo -> Rx -> Int) -> Memo -> Rx -> Int
ways' f m ([], []) = 1
ways' f m ([], [x]) = 0
ways' f m (s, []) = if none '#' s then 1 else 0
ways' f m (('.':rs), xs) = f m (rs, xs)
ways' f m (('?':rs), xs) = f m (rs, xs) + f m (('#':rs), xs)
ways' f m (s, (x:rx)) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = f m ((drop (x + 1) s), rx)
ways' _ _ _ = 0

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only, none :: Char -> String -> Bool
only c = all (== c) . nub
none c = not . any (== c) . nub
