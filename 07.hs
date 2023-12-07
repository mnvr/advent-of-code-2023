import Data.List (sortBy)
main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseCards

type Card = (String, Int)

parseCards :: String -> [Card]
parseCards = map (fmap read . span (/= ' ')) . lines

compareCards :: Card -> Card -> Ordering
compareCards (d, _) (d', _) = GT

-- p1 :: [Card] ->
p1 = sortBy compareCards
