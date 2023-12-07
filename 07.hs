import Data.List (sortBy, nub, elemIndex, sortOn)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down))

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseHands

type Hand = (String, Int)

parseHands :: String -> [Hand]
parseHands = map (fmap read . span (/= ' ')) . lines

data HandAttr = HandAttr { htype :: HandType, hvalue :: Int }
    deriving (Show, Eq, Ord)

data HandType = Five | Four | Full | Three | Two | One | High
    deriving (Show, Eq, Ord)

handType :: String -> HandType
handType s = case (length . nub) s of
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

handValue :: [Char] -> String -> Int
handValue labelStrength =
    foldl1 (\v i -> v * 100 + i) . map (fromJust . (`elemIndex` labelStrength))

labelStrength :: [Char]
labelStrength = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

p1 :: [Hand] -> Int
p1 = winnings (\(s, _) -> HandAttr (handType s) (handValue labelStrength s))

winnings :: (Hand -> HandAttr) -> [Hand] -> Int
winnings mkAttr = sum . zipWith (*) [1..] . map snd . sortOn (Down . mkAttr)
