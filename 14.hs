import Control.Arrow ((&&&))
import Data.List
import Data.Bifunctor
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . parse

parse = sum . map g . countdown . transpose . map f . transpose . lines
 where
    f [] = []
    f s = let (a, b) = span (/= '#') s in (reverse $ sort a) ++ (let (c, d) = break (/= '#') b in c ++ (if null d then [] else f d))
    countdown xs = zip [length xs..] xs
    -- g (i, s) = (i, (length $ filter (== 'O') s), s)--unt id -- g' (length ls) ls
    g (i, s) = i * (length $ filter (== 'O') s)--unt id -- g' (length ls) ls
    -- g' n ls = map (h n) ls
