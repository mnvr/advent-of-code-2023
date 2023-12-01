import System.IO.Error (tryIOError)
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = getLines >>= (print . sum . fmap parse)

getLines :: IO [String]
getLines = tryIOError getLine >>= e
  where e (Left _) = pure []
        e (Right s) = fmap (s :) getLines

parse :: String -> Int
parse s = head xs * 10 + last xs
    where xs = numbers s matchers

numbers :: String -> [Matcher] -> [Int]
numbers [] _ = []
numbers (c:s) ms = if isDigit c then digitToInt c : numbers s ms else numbers s ms

-- A matcher is a map of strings to integers. When the string becomes empty,
-- we've "matched" the corresponding integer, otherwise the matcher should be
-- reset to the original state.
data Matcher = Matcher { remaining :: String, result :: Integer, original :: String }

-- 'matchOrConsume' takes a character and a current set of matchers, and if it
-- matches a digit based on the current set, it returns the digit. In either
-- case, it also returns the updated matchers.
matchOrConsume :: Char -> [Matcher] -> (Maybe Int, [Matcher])
matchOrConsume c ms
    | isDigit c = (Just $ digitToInt c, ms)
    | otherwise = (Nothing, ms)

matchers :: [Matcher]
matchers = zipWith (\ s i -> Matcher s i s) spelled [0..]

spelled :: [String]
spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
