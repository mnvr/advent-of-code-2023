{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Control.Monad
import Data.Map qualified as M
import Text.Parsec hiding (State)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseCards

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

winsRec :: [Card] -> Int -> State (M.Map Int [Int]) [Int]
winsRec cards i = gets (M.lookup i) >>= \case
    Just xs -> pure xs
    Nothing -> case wins cards i of
                 [] -> modify (M.insert i []) >> pure []
                 ys -> do
                    result' <- foldM f [] ys
                    let result = concat (ys : result')
                    modify (M.insert i result) >> pure result
                  where f prev y = (: prev) <$> winsRec cards y

allWins :: [Card] -> State (M.Map Int [Int]) [Int]
allWins cards = foldM f [] [0..length cards - 1]
  where f w i = winsRec cards i >>= \extra ->
         pure ((1 + length extra) : w)

p2 :: [Card] -> Int
p2 cards = sum $ evalState (allWins cards) M.empty
