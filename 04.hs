import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = interact $ (++ "\n") . show . parseCards

data Card = Card { winning :: [Int], have :: [Int] } deriving (Show)

-- parseCards :: String -> [Card]
parseCards s = case parse cards "" (head (lines s)) of
    Left err -> error (show err)
    Right g -> g
  where
    cards = card -- `sepBy` newline
    card = string "Card" *> space *> num *> char ':' *> space *> nums
    num :: Parser Int
    num = read <$> many1 digit
    nums = num `sepBy` space
    winning = winning2 `endBy` string "|"
    winning2 = num `sepBy` space -- many1 (space *> num) `endBy` (space *> char '|')
    rest = many1 (space *> num)
