import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = interact $ (++ "\n") . show . parseCards

data Card = Card { winning :: [Int], have :: [Int] } deriving (Show)

parseCards :: String -> [Card]
parseCards s = case parse cards "" s of
    Left err -> error (show err)
    Right g -> g
  where
    cards = many1 card
    card = do
        winning <- prelude *> nums <* char '|'
        Card winning <$> nums
    prelude = string "Card" *> space *> num *> char ':'
    num = read <$> many1 digit
    nums = many1 (between (many space) (many space) num)