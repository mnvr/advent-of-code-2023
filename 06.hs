import Text.Parsec
import Data.Char (isDigit)

main :: IO ()
main = interact $ (++ "\n") . show .
  ((,) <$> p1 . parseRaces <*> p2 . parseAsSingleRace)

data Races = Races { times:: [Int], distances :: [Int] } deriving Show

parseRaces :: String -> Races
parseRaces s = case parse races "" s of
    Left e -> error (show e)
    Right v -> v
  where races = Races <$> (nums <* newline) <*> nums
        num = read <$> many1 digit
        nums = many1 (between ignored ignored num)
        ignored = skipMany (noneOf ('\n' : ['0'..'9']))

parseAsSingleRace :: String -> Races
parseAsSingleRace s = parseRaces $ filter (\c -> isDigit c || c == '\n') s

p1 :: Races -> Int
p1 Races { times, distances } = product $ zipWith waysToWin times distances

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
p2 (Races [rt] [d]) = lastB rt - firstB 1 + 1
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
