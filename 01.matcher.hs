import System.IO.Error (tryIOError)
import Data.Char (isDigit, digitToInt)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Bifunctor (first)

-- WARNING! Incorrect solution, doesn't work.

main :: IO ()
main = getLines >>= (print . sum . fmap parse)

getLines :: IO [String]
getLines = tryIOError getLine >>= e
  where e (Left _) = pure []
        e (Right s) = fmap (s :) getLines

parse :: String -> Int
parse s = head xs * 10 + head ys
    where xs = catMaybes $ numbers s matchers
          ys = catMaybes $ numbers (reverse s) reverseMatchers

numbers :: String -> [Matcher] -> [Maybe Int]
numbers [] _ = []
numbers (c:s) ms = let (d, ms') = matchOrConsume c ms in d : numbers s ms'

-- A matcher is a map of strings to integers. When the string becomes empty,
-- we've "matched" the corresponding integer, otherwise the matcher should be
-- reset to the original state.
data Matcher = Matcher { remaining :: String, num :: Int, original :: String }
    deriving Show

-- 'matchOrConsume' takes a character and a current set of matchers, and if it
-- matches a digit based on the current set, it returns the digit. In either
-- case, it also returns the updated matchers.
matchOrConsume :: Char -> [Matcher] -> (Maybe Int, [Matcher])
matchOrConsume c ms
    | isDigit c = (Just $ digitToInt c, ms)
    | otherwise = first (listToMaybe . catMaybes) $ unzip $ map (`match` c) ms

match :: Matcher -> Char -> (Maybe Int, Matcher)
match m@(Matcher (r:rs) num original) c = (if isMatch then Just num else Nothing, m')
  where m' = if null rs || c /= r then m { remaining = original } else m { remaining = rs }
        isMatch = c == r && null rs

matchers :: [Matcher]
matchers = makeMatchers spelled

reverseMatchers :: [Matcher]
reverseMatchers = makeMatchers (map reverse spelled)

makeMatchers :: [String] -> [Matcher]
makeMatchers = zipWith (\ i s -> Matcher s i s) [1..]

spelled :: [String]
spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
