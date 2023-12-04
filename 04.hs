import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Map qualified as M
import Control.Applicative (liftA2)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) p1 p2 . parseCards

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

winrecmemo :: [Card] -> (Int, M.Map Int [Int]) -> ([Int], M.Map Int [Int])
winrecmemo cards (i, memo) = case M.lookup i memo of
    Just xs -> (xs, memo)
    Nothing -> case wins cards i of
                 [] -> ([], M.insert i [] memo)
                 ys -> let (result', memo') = foldl f ([], memo) ys
                           result = concat (ys : result')
                       in (result, M.insert i result memo')
                         where f (prev, m) y = let (z2, m2) = winrecmemo cards (y, m)
                                               in (z2 : prev, m2)

p2 :: [Card] -> Int
p2 cards = fst $ foldl f (0, M.empty) [0..length cards - 1]
  where f (s, m) i = let (extra, m') = winrecmemo cards (i, m)
                        in (s + 1 + length extra, m')
