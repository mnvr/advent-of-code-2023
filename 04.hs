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
    card = do
        n1 <- string "Card" *> space *> num *> char ':' *> nums <* char '|'
        n2 <- nums <* eof
        pure (n1, n2)
    -- <*> nums <* eof
    num :: Parser Int
    num = read <$> many1 digit
    nums = many1 (between (many space) (many space) num)
    -- winning = nums `endBy` string "|"
    -- winning = manyTill (num <* space) eof -- (char '|')
    -- nums = num `sepBy` (many space) -- many1 (space *> num) `endBy` (space *> char '|')
    rest = many1 (space *> num)
