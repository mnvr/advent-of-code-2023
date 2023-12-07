main :: IO ()
main = interact $ (++ "\n") . show . parseCards

type Card = (String, Int)

parseCards :: String -> [Card]
parseCards = map (fmap read . span (/= ' ')) . lines
