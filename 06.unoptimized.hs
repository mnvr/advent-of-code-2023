import Text.Parsec
import Data.Char (isDigit)
import Data.List (find)

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

p2 :: Races -> Int
p2 = p2A

-- Unoptimized versions.
--
-- These take longer than I'd expect. With runghc, they take 15 seconds. When
-- complied with "-O2", both these take around 0.5 seconds.
--
-- This is not too surprising because runghc runs the code using the interpreter
-- without any optimizations. But still, mildly interesting to find an example
-- of such drastic difference between interpreted and optimized code.

-- Finding (only) the first and last winning indices, sequentially.

p2A :: Races -> Int
p2A (Races [t] [d]) = case (firstWayToWin t d, lastWayToWin t d) of
    (Just f, Just l) -> l - f + 1
    _ -> 0

firstWayToWin :: Int -> Int -> Maybe Int
firstWayToWin rt d = find (\t -> (rt `holdFor` t) > d) [0..rt]

lastWayToWin :: Int -> Int -> Maybe Int
lastWayToWin rt d = find (\t -> (rt `holdFor` t) > d) [rt,(rt-1)..0]

-- Writing a custom fold did not help either, au contraire made things slightly
-- worse.

p2B :: Races -> Int
p2B (Races [t] [d]) = case boundaryWaysToWin t d of
    (Just f, Just l, _) -> l - f + 1
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
          | otherwise = if isWin (rt - i) then f (Nothing, Just (rt - i), i) else f (Nothing, Nothing, i + 1)
