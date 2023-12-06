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
p2 (Races [t] [d]) = case (firstWayToWin t d, lastWayToWin t d) of
    (Just f, Just l) -> l - f
    _ -> 0

firstWayToWin :: Int -> Int -> Maybe Int
firstWayToWin rt d = find (> d) $ map (rt `holdFor`) [0..rt]

lastWayToWin :: Int -> Int -> Maybe Int
lastWayToWin rt d = find (> d) $ map (rt `holdFor`) (reverse [0..rt])
