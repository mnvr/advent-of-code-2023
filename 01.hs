import System.IO.Error (tryIOError)
import Data.Char (isDigit, digitToInt)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Bifunctor (first)
import Data.List (inits, find, isPrefixOf)

main :: IO ()
main = getLines >>= (print . sum . fmap parse)

getLines :: IO [String]
getLines = tryIOError getLine >>= e
  where e (Left _) = pure []
        e (Right s) = fmap (s :) getLines

parse :: String -> Int
parse s = left s * 10 + right (reverse s)

left :: String -> Int
left s@(c:s') = if isDigit c then digitToInt c else
    case find (\(p, i) -> p `isPrefixOf` s) (zip spelled [1..]) of
        Nothing -> left s'
        (Just (s, i)) -> i

right :: String -> Int
right s@(c:s') = if isDigit c then digitToInt c else
    case find (\(p, i) -> p `isPrefixOf` s) (zip (map reverse spelled) [1..]) of
        Nothing -> right s'
        (Just (s, i)) -> i

spelled :: [String]
spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
