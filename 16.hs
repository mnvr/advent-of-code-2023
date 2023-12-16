import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Grid = M.Map (Int, Int) Char

parse :: String -> Grid
parse = M.fromList . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)

p1 = id
