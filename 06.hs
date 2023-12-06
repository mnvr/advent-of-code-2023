import Text.Parsec

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseRaces

data Races = Races { times:: [Int], distance :: [Int] }

parseRaces :: String -> Races
parseRaces s = case parse races "" s of
    Left e -> error (show e)
    Right v -> v
  where races = string "Time"

-- p1 :: Races -> Int
p1 = id
