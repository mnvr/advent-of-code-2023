import Data.List (sortBy, nub)
import Data.Bifunctor (first)
import Data.Monoid -- (Sum)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseHands

type Hand = ((String, HandType), Int)

data HandType = Five | Four | Full | Three | Two | One | High deriving Show

cardCounts :: String -> [(Char, Int)]
cardCounts = foldl (\as c -> fmap (+1) ((lookup c as) <> Just (Sum 1)) : as) []

mkHandType :: String -> HandType
mkHandType _ = Five
-- mkCardType s = case length u of
    --   1 -> Five
    --   2 ->
    -- where u = nub s

parseHands :: String -> [Hand]
parseHands = map (first (\x -> (x, mkHandType x)) . fmap read . span (/= ' ')) . lines

compareHands :: Hand ->Hand -> Ordering
compareHands (d, _) (d', _) = GT

p1 :: [Hand] -> [Hand]
-- p1 = sortBy compareCards
-- p1 xs = map (first . first nub) xs
p1 xs = xs
