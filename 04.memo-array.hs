{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Data.Map qualified as M
import Text.Parsec
import Data.Array qualified as A
import Data.Array ((!), (//))

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

memo :: (MonadState (A.Array i (Maybe b)) m, A.Ix i) => i -> m b -> m b
memo i compute = gets (! i) >>= \case
  Just v -> pure v
  Nothing -> compute >>= \r -> modify (// [(i, Just r)]) >> pure r

winsRec :: MonadState (A.Array Int (Maybe [Int])) m => [Card] -> Int -> m [Int]
winsRec cards i = memo i (_winsRec cards i)

_winsRec :: MonadState (A.Array Int (Maybe [Int])) m => [Card] -> Int -> m [Int]
_winsRec cards i = case wins cards i of
  [] -> pure []
  ys -> do
    r' <- let f prev y = (: prev) <$> winsRec cards y in foldM f [] ys
    pure $ concat (ys : r')

allWins :: MonadState (A.Array Int (Maybe [Int])) m => [Card] -> m [Int]
allWins cards = foldM f [] [0..length cards - 1]
  where f w i = winsRec cards i >>= \extra ->
         pure ((1 + length extra) : w)

p2 :: [Card] -> Int
p2 cards = sum $ evalState (allWins cards) nothings
  where nothings = mkArray (length cards) Nothing
        mkArray n x = let bs = (0, n-1) in A.array bs [(i,x) | i <- A.range bs]

