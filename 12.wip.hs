{-# OPTIONS_GHC -Wno-all #-}

import Data.List (intercalate)
import Data.Map qualified as M

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
rc = 1 :: Int

unfold :: [String] -> [String]
unfold [s, xs] = [intercalate "?" (replicate rc s), intercalate "," (replicate rc xs)]

p1 = snd . foldl count ((M.empty, M.empty, M.empty), 0)
  where count (c@(cm, em, vm), s) (ss, xs) =
           let ((em', vm'), es) = expand (em, vm) ss
           in foldl f ((cm, em', vm'), s) es
         where f (c, s) e = let (c', i) = consume c e xs in (c', s + i)

type CM = M.Map ([String], [Int]) Int
consume :: (CM, EM, VM) -> [String] -> [Int] -> ((CM, EM, VM), Int)
consume c [] [] = (c, 1)
consume c _  [] = (c, 0)
consume c []  _ = (c, 0)
consume c@(cm, em, vm) (s:ss) (x:xs) = let k = (s:ss, x:xs) in
  case M.lookup k cm of
    Just v -> (c, v)
    Nothing ->
      let ((cm', em', vm'), v) = if length s /= x then (c, 0)
                                 else consume c ss xs
      in ((M.insert k v cm', em', vm'), v)

type EM = M.Map [String] [[String]]
expand :: (EM, VM) -> [String] -> ((EM, VM), [[String]])
expand c [] = (c, [[]])
expand c@(m, vm) k@(s:ss)
  | '?' `notElem` s = case M.lookup k m of
    Just v -> (c, v)
    Nothing ->
      let ((m', vm'), es) = expand c ss
          v = map (s:) es
      in ((M.insert k v m', vm'), v)
  | otherwise = case M.lookup k m of
    Just v -> (c, v)
    Nothing ->
      let ((m', vm1), es) = expand c ss
          (vm'', vs) = variations vm1 s
          v = concatMap (\z -> [words v ++ z | v <- vs]) es
      in ((M.insert k v m', vm''), v)

type VM = M.Map String [String]
variations :: VM -> String -> (VM, [String])
variations m [] = (m, [[]])
variations m k@('#':s) = case M.lookup k m of
  Just v -> (m, v)
  Nothing -> let (m', vs) = variations m s
                 v = map ('#':) vs
             in (M.insert k v m', v)
variations m k@('?':s) = case M.lookup k m of
  Just v -> (m, v)
  Nothing -> let (m', vs) = variations m s
                 v = concatMap (\s -> ['#':s, ' ':s]) vs
             in (M.insert k v m', v)
