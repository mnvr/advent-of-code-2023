-- WIP!
-- A simpler, barebones version of the solution with a foldr.

import Data.Bifunctor
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseCards

-- nums :: String -> [Int]
nums =  catMaybes . nums'
nums' :: String -> [Maybe Int]
nums' [] = []
nums' s = let (a, b) = break isSpace s in (readMaybe a :: Maybe Int) : nums' (dropWhile isSpace b)

-- parseCards :: String -> [([Int], [Int])]
parseCards s = map (bimap nums nums . span (/= '|')) (lines s)


  -- map parseCard $ lines s
  -- where parseCard s' = bimap nums nums $ bimap tail tail $ span (/= '|') $ dropWhile (/= ':') s'
  --       nums s' = id s'

p1 = id
p2 = p1
