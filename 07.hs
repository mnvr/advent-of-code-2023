import Data.List (sortOn, nub, elemIndex, delete, minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down))
import Data.Function (on)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parseHands

type Hand = (String, Int)

parseHands :: String -> [Hand]
parseHands = map (fmap read . span (/= ' ')) . lines

data HandAttr = HandAttr { htype :: HandType, hvalue :: Int }
    deriving (Show, Eq, Ord)

data HandType = Five | Four | Full | Three | Two | One | High
    deriving (Show, Eq, Ord)

handType :: Hand -> HandType
handType (s, _) = case (length . nub) s of
      1 -> Five
      2 -> if maxCardCount s == 4 then Four else Full
      3 -> if maxCardCount s == 3 then Three else Two
      4 -> One
      5 -> High

maxCardCount :: String -> Int
maxCardCount = maximum . map snd . cardCounts

cardCounts :: String -> [(Char, Int)]
cardCounts = foldl (\as c -> incr as c : as) [] where
  incr as c = case lookup c as of Nothing -> (c, 1); Just i -> (c, i + 1)

handValue :: [Char] -> Hand -> Int
handValue labelStrength (s, _) =
    foldl1 (\v i -> v * 100 + i) . map (fromJust . (`elemIndex` labelStrength)) $ s

cards :: [Char]
cards = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

p1 :: [Hand] -> Int
p1 = winnings (\h -> HandAttr (handType h) (handValue cards h))

winnings :: (Hand -> HandAttr) -> [Hand] -> Int
winnings mkAttr = sum . zipWith (*) [1..] . map snd . sortOn (Down . mkAttr)

p2 :: [Hand] -> Int
p2 = winnings (\h -> HandAttr (bestHandType h) (handValue cardsJ h))
  where cardsJ = delete 'J' cards ++ ['J']
        bestHandType = handType . highestRanked . jokerVariants

highestRanked :: [Hand] -> Hand
highestRanked = minimumBy (compare `on` handType)

jokerVariants :: Hand -> [Hand]
jokerVariants h@(s, v) = if hasJoker h then variations else [h]
  where variations = [(replaceJ c, v) | c <- cards]
        replaceJ r = map (\c -> if c == 'J' then r else c) s

hasJoker :: Hand -> Bool
hasJoker (s, _) = 'J' `elem` s
