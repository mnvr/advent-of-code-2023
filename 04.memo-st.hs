{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Data.Map qualified as M
import Text.Parsec
import Control.Monad.ST -- qualified as ST
import Data.Array.ST -- qualified as SA

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

-- memo :: (MonadState (M.Map k b) m, Ord k) => k -> m b -> m b
memo memoTable i compute = readArray memoTable i >>= \case
  Just v -> pure v
  Nothing -> compute >>= \r -> writeArray memoTable i (Just r) >> pure r

-- winsRec :: MonadState (M.Map Int [Int]) m => [Card] -> Int -> m [Int]
winsRec cards memoTable i = memo memoTable i (_winsRec cards memoTable i)

-- -- Note - for recursive calls, this "actual" version must call the memoized, non
-- -- underscore version for the memoization to kick in.
-- _winsRec :: MonadState (M.Map Int [Int]) f => [Card] -> Int -> f [Int]
_winsRec cards memoTable i = case wins cards i of
  [] -> pure []
  ys -> do
    r' <- let f prev y = (: prev) <$> winsRec cards memoTable y in foldM f [] ys
    pure $ concat (ys : r')

-- allWins :: MonadState (M.Map Int [Int]) m => [Card] -> m [Int]
allWins cards memoTable = foldM f [] [0..length cards - 1]
  where f w i = winsRec cards memoTable i >>= \extra ->
         pure ((1 + length extra) : w)

p2 :: [Card] -> Int
p2 cards = sum $ runST $ do
  memoTable <- newArray (0, length cards - 1) Nothing :: ST a (STArray a Int (Maybe [Int]))
  allWins cards memoTable
