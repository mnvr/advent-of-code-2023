import Control.Arrow ((&&&))
import Data.List

main :: IO ()
main = interact $ (++ "\n") . unlines . parse

parse = transpose . map f . transpose . lines
 where
    f = id
