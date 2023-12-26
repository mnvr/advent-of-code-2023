import Data.Map qualified as M
import Data.Map ((!))
import Debug.Trace (trace)

main :: IO ()
main = interact $ (++ "\n") . show . p2 . parse

type Index = (Int, Int)
type Garden = M.Map Index Char

parse :: String -> Garden
parse = foldl row M.empty . enum . lines
  where row m (y, line) = foldl cell m (enum line)
          where cell m (x, c) = M.insert (x, y) (so c) m
                so c = if c == 'S' then 'O' else c

enum :: [a] -> [(Int, a)]
enum = zip [0..]

occupied :: Garden -> Int
occupied = length . filter (== 'O') . M.elems

blank :: Garden -> Garden
blank = M.map (\c -> if c == 'O' then '.' else c)

p1 :: Garden -> Int
p1 garden = step 64 garden
  where
    step :: Int -> Garden -> Int
    step 0 g = occupied g
    step i g = (step (i - 1) . foldl f (blank g) . M.keys) g
      where f m k = case g ! k of
              'O' -> foldl (\m k' -> M.insert k' 'O' m) m (next k)
              _  -> m
    next :: Index -> [Index]
    next (x, y) = filter valid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    valid :: Index -> Bool
    valid k = case M.lookup k garden of
        Nothing -> False
        (Just '#') -> False
        _ -> True

p2 :: Garden -> Int
p2 garden = p2' garden bounds
  where bounds = maximum (M.keys garden)

p2' :: Garden -> (Index) -> Int
p2' garden (mx, my) = trace ("bounds " ++ show (mx, my)) $ step 64 garden
  where
    step :: Int -> Garden -> Int
    step 0 g = occupied g
    step i g = trace ("step " ++ show (64 - i) ++ " occupied " ++ show (occupied g)) $ (step (i - 1) . foldl f (blank g) . M.keys) g
      where f m k = case g ! k of
              'O' -> foldl (\m k' -> M.insert k' 'O' m) m (next k)
              _  -> m
    occupied :: Garden -> Int
    occupied = length . filter (== 'O') . M.elems
    blank :: Garden -> Garden
    blank = M.map (\c -> if c == 'O' then '.' else c)
    next :: Index -> [Index]
    next (x, y) = filter valid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    valid :: Index -> Bool
    valid k = case M.lookup k garden of
        Nothing -> False
        (Just '#') -> False
        _ -> True
