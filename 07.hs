import Data.List (sortBy, nub, elemIndex)
import Data.Bifunctor (first)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseHands

type Hand = ((String, HandType), Int)

data HandType = Five | Four | Full | Three | Two | One | High deriving (Show, Eq, Ord)

parseHands :: String -> [Hand]
parseHands = map (first (\x -> (x, mkHandType x)) . fmap read . span (/= ' ')) . lines

mkHandType :: String -> HandType
mkHandType s = case (length . nub) s of
      1 -> Five
      2 -> if maxCardCount s == 4 then Four else Full
      3 -> if maxCardCount s == 3 then Three else Two
      4 -> One
      5 -> High

cardCounts :: String -> [(Char, Int)]
cardCounts = foldl (\as c -> incr as c : as) [] where
  incr as c = case lookup c as of Nothing -> (c, 1); Just i -> (c, i + 1)

maxCardCount :: String -> Int
maxCardCount = maximum . map snd . cardCounts

compareHands :: Hand -> Hand -> Ordering
compareHands ((s,t), _) ((s',t'), _) =
    let o = compare t t' in if o == EQ then compareCards s s' else o

compareCards :: [Char] -> [Char] -> Ordering
compareCards s s' = compare (labels s) (labels s')
  where labels = map (`elemIndex` labelStrength)

labelStrength :: [Char]
labelStrength = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

p1 :: [Hand] -> Int
p1 = winnings compareHands

winnings :: (Hand -> Hand -> Ordering) -> [Hand] -> Int
winnings cmp = sum . zipWith (*) [1..] . map snd . sortBy (flip cmp)
