import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (sort)

main :: IO ()
main = interact $ (++ "\n") . show . p2 . parseCards

data Card = Card { winning :: [Int], have :: [Int] } deriving (Show)

parseCards :: String -> [Card]
parseCards s = case parse cards "" s of
    Left err -> error (show err)
    Right v -> v
  where
    cards = many1 card
    card = Card <$> (prelude *> nums <* char '|') <*> nums
    prelude = string "Card" *> spaces *> num *> char ':'
    num = read <$> many1 digit
    nums = many1 (between spaces spaces num)

matches :: Card -> Int
matches Card { winning, have } = length $ filter (`elem` have) winning

points :: Card -> Int
points = score . matches where score 0 = 0
                               score n = 2 ^ (n - 1)

p1 :: [Card] -> Int
p1 = sum . map points

wins :: [Card] -> Int -> [Int]
wins cards i = case matches (cards !! i) of
    0 -> []
    n -> [(i+1)..(i+n)]

winrec :: [Card] -> Int -> [Int]
winrec cards i = case wins cards i of
    [] -> []
    xs -> xs ++ concatMap (winrec cards) xs

p2 :: [Card] -> Int
p2 cs = sum $ map ((+1) . length . winrec cs) [0..length cs - 1]
-- p2 cs = (\x -> (length x, sort x)) $ winrec cs 0
