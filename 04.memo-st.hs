{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Data.Map qualified as M
import Text.Parsec
import Control.Monad.ST
import Data.Array.ST

-- ST stands for "State Thread"
--
-- It is a way to imperatively modify state within a controlled scope. Here we
-- use it to efficiently implement the memo lookup table for our recursive
-- solution to part 2.

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

memo :: Ix i => STArray s i (Maybe b) -> i -> ST s b -> ST s b
memo memoTable i compute = readArray memoTable i >>= \case
  Just v -> pure v
  Nothing -> compute >>= \r -> writeArray memoTable i (Just r) >> pure r

winsRec :: [Card] -> STArray s Int (Maybe [Int]) -> Int -> ST s [Int]
winsRec cards memoTable i = memo memoTable i (_winsRec cards memoTable i)

_winsRec :: [Card] -> STArray s Int (Maybe [Int]) -> Int -> ST s [Int]
_winsRec cards memoTable i = case wins cards i of
  [] -> pure []
  ys -> do
    r' <- let f prev y = (: prev) <$> winsRec cards memoTable y in foldM f [] ys
    pure $ concat (ys : r')

allWins :: [Card] -> STArray s Int (Maybe [Int]) -> ST s [Int]
allWins cards mt = foldM f [] [0..length cards - 1]
  where f w i = winsRec cards mt i >>= \extra ->
         pure ((1 + length extra) : w)

p2 :: [Card] -> Int
p2 cards = sum $ runST $ do
  memoTable <- newArray (0, length cards - 1) Nothing :: ST a (STArray a Int (Maybe [Int]))
  allWins cards memoTable
