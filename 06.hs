import Text.Parsec

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseRaces

data Races = Races { times:: [Int], distance :: [Int] } deriving Show

-- parseRaces :: String -> Races
parseRaces s = case parse races "" s of
    Left e -> error (show e)
    Right v -> v
  where races = Races <$> (nums <* newline) <*> nums
        num = read <$> many1 digit
        nums = many1 (between ignored ignored num)
        ignored = skipMany (noneOf ('\n' : ['0'..'9']))

-- p1 :: Races -> Int
p1 = id
