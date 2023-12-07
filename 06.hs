import Data.Char (isDigit, isSpace)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

main :: IO ()
main = interact $ (++ "\n") . show .
  ((,) <$> p1 . parseRaces <*> p2 . parseAsSingleRace)

type Races = ([Int], [Int])

parseRaces :: String -> Races
parseRaces s = case map nums (lines s) of x:y:_ -> (x,y)

nums :: String -> [Int]
nums = catMaybes . ms where
  ms [] = []
  ms s = let (a, b) = break isSpace (dropWhile isSpace s) in readMaybe a : ms b

parseAsSingleRace :: String -> Races
parseAsSingleRace s = parseRaces $ filter (\c -> isDigit c || c == '\n') s

p1 :: Races -> Int
p1 (times, distances) = product $ zipWith waysToWin times distances

-- How many ways can we win a race of time t and record distance d?
waysToWin :: Int -> Int -> Int
waysToWin rt d = length $ filter (> d) $ map (rt `holdFor`) [0..rt]

-- What distance do we cover if we hold for time t out of total race time rt?
holdFor :: Int -> Int -> Int
holdFor rt t = remainingTime * speed
  where speed = t
        remainingTime = rt - t

-- Binary search

p2 :: Races -> Int
p2 ([rt], [d]) = lastB rt - firstB 1 + 1
  where
    check t = (rt `holdFor` t) > d
    firstB lb = first' lb (firstBound lb)
    firstBound t = if check t then t else firstBound (t * 2)
    first' p q
      | p == q = p
      | otherwise = let m = p + ((q - p) `div` 2)
                    in if check m then first' p m else first' (m + 1) q
    lastB ub = last' (lastBound ub) ub
    lastBound t = if check t then t else lastBound (t `div` 2)
    last' p q
      | p == q = p
      | p + 1 == q = if check q then q else p
      | otherwise = let m = p + ((q - p) `div` 2)
                    in if check m then last' m q else last' p (m - 1)
