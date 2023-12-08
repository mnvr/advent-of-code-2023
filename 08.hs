import Data.Bifunctor (bimap, Bifunctor (second))

main :: IO ()
main = interact $ (++ "\n") . show . parse

parse = bimap head (map instr) . splitAt 2. lines
  where instr = second (pair . drop 3) . label
        pair = second (fst . label . drop 2) . label . tail
        label = splitAt 3

