import Text.Parsec
import Data.Char (isDigit)
import Data.List (find)

main :: IO ()
main = interact $ (++ "\n") . show .
  ((,) <$> p1 . parseRaces <*> p1 . parseAsSingleRace)

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

p2 :: Races -> Int
p2 (Races [t] [d]) = case boundaryWaysToWin t d of
    (Just f, Just l, _) -> l - f
    _ -> 0

boundaryWaysToWin :: Int -> Int -> (Maybe Int, Maybe Int, Int)
boundaryWaysToWin rt d = f (Nothing, Nothing, 0)
  where isWin t = (rt `holdFor` t) > d
        check t = if (rt `holdFor` t) > d then Just t else Nothing
        f (Just x, Nothing, i) = let i' = rt - i in
          if isWin i' then (Just x, Just i', i) else f (Just x, Nothing, i + 1)
        f (Nothing, Just y, i) = if isWin i then (Just i, Just y, i) else f (Nothing, Just y, i + 1)
        f (Nothing, Nothing, i)
          | isWin i = if isWin (rt - i) then (Just i, Just (rt - i), i) else f (Just i, Nothing, i + 1)
          | otherwise = if isWin (rt - i) then (Just i, Just (rt - i), i) else f (Just i, Nothing, i + 1)

-- Unoptimized versions.

-- These take longer than I'd expect. With runghc, they take 15 seconds. It is
-- faster after compiling with GHC - it gets down to 1 second. With "-O2", it
-- becomes 0.4 second, about as fast as one would expect.
--
-- This is not too surprising, the relevant optimizations wouldn't have been
-- kicking in when running under runghc, but still, interesting to find an
-- example of this drastic difference between runghc'd and optimized code.
--
-- Fusing the find and map doesn't seem to have helped either.

p2' :: Races -> Int
p2' (Races [t] [d]) = case (firstWayToWin t d, lastWayToWin t d) of
    (Just f, Just l) -> l - f
    _ -> 0

firstWayToWin :: Int -> Int -> Maybe Int
firstWayToWin rt d = find (\t -> (rt `holdFor` t) > d) [0..rt]

lastWayToWin :: Int -> Int -> Maybe Int
lastWayToWin rt d = find (\t -> (rt `holdFor` t) > d) [rt,(rt-1)..0]

firstWayToWin' :: Int -> Int -> Maybe Int
firstWayToWin' rt d = find (> d) $ map (rt `holdFor`) [0..rt]

lastWayToWin' :: Int -> Int -> Maybe Int
lastWayToWin' rt d = find (> d) $ map (rt `holdFor`) (reverse [0..rt])
