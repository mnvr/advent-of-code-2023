import Data.List (nub, intercalate)
import Data.Map qualified as M

-- The manually memoized (without using the State monad) variation of 12.hs.
--
-- This builds up from step 1 (12.memo1.hs), and adds the actual map lookup and
-- insertion to memoize the results. The final version (12.hs) just abstracts
-- this handling under the State monad.

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
ways = snd . ways' memo M.empty
  where
    -- Uncomment this to see a version that doesn't do any memoization
    --memo m k = ways' memo m k
    -- This one does the lookup + insertion which serves as our memoization
    memo m k = case M.lookup k m of
      Just v -> (m, v)
      Nothing -> let (m', v) = ways' memo m k in (M.insert k v m', v)

ways' :: (Memo -> Rx -> (Memo, Int)) -> Memo -> Rx -> (Memo, Int)
ways' f m ([], []) = (m, 1)
ways' f m ([], [x]) = (m, 0)
ways' f m (s, []) = if none '#' s then (m, 1) else (m, 0)
ways' f m (('.':rs), xs) = f m (rs, xs)
ways' f m (('?':rs), xs) = let (m1, v1) = f m (rs, xs)
                               (m2, v2) = f m1 (('#':rs), xs)
                           in (m2, v1 + v2)
ways' f m (s, (x:rx)) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = f m ((drop (x + 1) s), rx)
ways' _ m _ = (m, 0)

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only, none :: Char -> String -> Bool
only c = all (== c) . nub
none c = not . any (== c) . nub
