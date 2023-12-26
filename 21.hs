import Data.Map qualified as M
import Data.Map ((!))

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Index = (Int, Int)
type Garden = M.Map Index Char

parse :: String -> Garden
parse = foldl row M.empty . enum . lines
  where row m (y, line) = foldl cell m (enum line)
          where cell m (x, c) = M.insert (x, y) (so c) m
                so c = if c == 'S' then 'O' else c

enum :: [a] -> [(Int, a)]
enum = zip [0..]

p1 :: Garden -> Int
p1 garden = step 6 garden
  where
    step :: Int -> Garden -> Int
    step 0 g = (length . filter (== 'O') . M.elems) g
    step i g = (step (i - 1) . foldl f (blank g) . M.keys) g
      where f m k = case g ! k of
              'O' -> foldl (\m k' -> M.insert k' 'O' m) m (next k)
              _  -> m
    blank :: Garden -> Garden
    blank = M.map (\c -> if c == 'O' then '.' else c)
    next :: Index -> [Index]
    next (x, y) = filter valid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    valid :: Index -> Bool
    valid k = case M.lookup k garden of
        Nothing -> False
        (Just '#') -> False
        _ -> True
