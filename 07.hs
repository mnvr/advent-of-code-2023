import Data.List (sortBy, nub)
import Data.Bifunctor (first)
import Data.Monoid -- (Sum)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseHands

type Hand = ((String, HandType), Int)

data HandType = Five | Four | Full | Three | Two | One | High deriving Show

cardCounts :: String -> [(Char, Int)]
cardCounts = foldl (\as c -> incr as c : as) [] where
  incr as c = case lookup c as of Nothing -> (c, 1); Just i -> (c, i + 1)

maxCardCount :: String -> Int
maxCardCount = maximum . map snd . cardCounts

mkHandType :: String -> HandType
mkHandType s = case (length . nub) s of
      1 -> Five
      2 -> if maxCardCount s == 4 then Four else Full
      3 -> if maxCardCount s == 3 then Three else Two
      4 -> One
      5 -> High

parseHands :: String -> [Hand]
parseHands = map (first (\x -> (x, mkHandType x)) . fmap read . span (/= ' ')) . lines

compareHands :: Hand ->Hand -> Ordering
compareHands (d, _) (d', _) = GT

-- p1 :: [Hand] -> []
-- p1 = sortBy compareCards
-- p1 xs = map (first . first nub) xs
-- p1 :: [Hand] -> [(String, [(Char, Int)])]
-- p1 :: [((a, String), b)] -> [(((a, String), b), [(Char, Int)])]
p1 = map (id &&& cardCounts . fst . fst) -- (cardCounts . fst)) -- (\x -> (x, cardCounts (snd $ fst x)))
