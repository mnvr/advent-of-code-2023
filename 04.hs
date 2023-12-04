{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Map qualified as M
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

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

winsRec :: [Card] -> Int -> [Int]
winsRec cards i = case wins cards i of
    [] -> []
    xs -> xs ++ concatMap (winsRec cards) xs

winsRecMemo :: [Card] -> (Int, M.Map Int [Int]) -> ([Int], M.Map Int [Int])
winsRecMemo cards (i, memo) = case M.lookup i memo of
    Just xs -> (xs, memo)
    Nothing -> case wins cards i of
                 [] -> ([], M.insert i [] memo)
                 ys -> let (result', memo') = foldl f ([], memo) ys
                           result = concat (ys : result')
                       in (result, M.insert i result memo')
                         where f (prev, m) y = let (z2, m2) = winsRecMemo cards (y, m)
                                               in (z2 : prev, m2)

allWins :: [Card] -> [Int]
allWins cards = fst $ foldl f ([], M.empty) [0..length cards - 1]
  where f (w, m) i = let (extra, m') = winsRecMemo cards (i, m)
                        in (1 + length extra : w, m')

winsRecMemoState :: [Card] -> Int -> State (M.Map Int [Int]) [Int]
winsRecMemoState cards i = gets ( M.lookup i) >>= \case
    Just xs -> pure xs
    Nothing -> case wins cards i of
                 [] -> modify (M.insert i []) >> pure []
                 ys -> do
                    result' <- foldM f [] ys
                    let result = concat (ys : result')
                    modify (M.insert i result) >> pure result
                  where f prev y = winsRecMemoState cards y >>= pure . (: prev)

allWinsState :: [Card] -> State (M.Map Int [Int]) [Int]
allWinsState cards = foldM f [] [0..length cards - 1]
  where f w i = winsRecMemoState cards i >>= \extra ->
         pure ((1 + length extra) : w)

p2 :: [Card] -> Int
-- p2 = sum . allWins
p2 cards = sum $ evalState (allWinsState cards) M.empty
