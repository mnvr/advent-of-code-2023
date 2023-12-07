import Data.List (sortBy, nub, elemIndex, find)
import Data.Bifunctor (first)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe, fromJust)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseHands

type Hand = ((String, HandType), Int)

data HandType = Five | Four | Full | Three | Two | One | High deriving (Show, Eq, Ord)

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

compareHands :: Hand -> Hand -> Ordering
compareHands ((s,t), _) ((s',t'), _) = let ot = compare t t' in
    if ot /= EQ then ot else fromMaybe EQ $ find (/= EQ) (zipWith compareLabel s s')
    where compareLabel c c' = compare (labelIndex c) (labelIndex c')
          labelIndex c = fromJust (elemIndex c labelStrength)

labelStrength :: [Char]
labelStrength = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

p1 :: [Hand] -> Int
p1 = sum . map score . enumerate . sorted
  where sorted  = sortBy (flip compareHands)
        score (i, (_, j)) = (i + 1) * j
