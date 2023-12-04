import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseCards

data Card = Card { winning :: [Int], have :: [Int] } deriving (Show)

parseCards :: String -> [Card]
parseCards s = case parse cards "" s of
    Left err -> error (show err)
    Right v -> v
  where
    cards = many1 card
    card = Card <$> (prelude *> nums <* char '|') <*> nums
    prelude = string "Card" *> space *> num *> char ':'
    num = read <$> many1 digit
    nums = many1 (between (many space) (many space) num)

points :: Card -> Int
points Card { winning, have } = pt . length $ filter (`elem` have) winning
  where pt 0 = 0
        pt n = 2 ^ (n - 1)

p1 :: [Card] -> Int
p1 = sum . map points
