import Data.List (sortBy, nub)
import Data.Bifunctor (first)
main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseCards

type Card = ((String, CardType), Int)

data CardType = Five | Four | Full | Three | Two | One | High deriving Show

mkCardType :: String -> CardType
mkCardType s = High

parseCards :: String -> [Card]
parseCards = map (first (\x -> (x, mkCardType x)) . fmap read . span (/= ' ')) . lines

compareCards :: Card -> Card -> Ordering
compareCards (d, _) (d', _) = GT

p1 :: [Card] -> [Card]
-- p1 = sortBy compareCards
-- p1 xs = map (first . first nub) xs
p1 xs = xs
