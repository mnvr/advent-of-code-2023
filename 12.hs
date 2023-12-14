{-# LANGUAGE LambdaCase #-}

import Data.List (nub, intercalate)
import Data.Map qualified as M
import Control.Arrow ((&&&))
import Control.Monad.State

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

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
ways s = flip evalState M.empty . memo ways' s
  where
    memo mf s xs = let key = (s, xs) in gets (M.lookup key) >>= \case
      Just v -> pure v
      Nothing -> mf (memo mf) s xs >>= \v -> modify (M.insert key v) >> pure v

type MT = M.Map (String, [Int]) Int

ways' :: (String -> [Int] -> State MT Int) -> String -> [Int] -> State MT Int
ways' f [] [] = pure 1
ways' f [] [x] = pure 0
ways' f s [] = pure (if none '#' s then 1 else 0)
ways' f ('.':rs) xs = f rs xs
ways' f ('?':rs) xs = f rs xs >>= \a -> f ('#':rs) xs >>= \b -> pure (a + b)
ways' f s (x:rx) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = f (drop (x + 1) s) rx
ways' _ _ _= pure 0

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Char -> String -> Bool
only c = all (== c) . nub

none :: Char -> String -> Bool
none c = not . any (== c) . nub
