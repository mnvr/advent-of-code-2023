import Control.Arrow ((&&&))
import Data.List
import Data.Bifunctor
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . unlines . parse

parse =  transpose . map id . map f . transpose . lines
 where
    f [] = []
    f s = let (a, b) = span (/= '#') s in (reverse $ sort a) ++ (let (c, d) = break (/= '#') b in c ++ (if null d then [] else f d))
