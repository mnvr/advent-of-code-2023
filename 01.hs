import System.IO.Error (tryIOError)
import Data.Char (isDigit, digitToInt)
import Data.List (inits, find, isPrefixOf)

main :: IO ()
main = getLines >>= (print . sum . fmap parse)

getLines :: IO [String]
getLines = tryIOError getLine >>= e
  where e (Left _) = pure []
        e (Right s) = fmap (s :) getLines

parse :: String -> Int
parse s = first s spelled * 10 + first (reverse s) (map reverse spelled)

first :: String -> [String] -> Int
first s@(c:s') ms = if isDigit c then digitToInt c else
    case find ((`isPrefixOf` s) . fst) (zip ms [1..]) of
        Nothing -> first s' ms
        (Just (_, i)) -> i

spelled :: [String]
spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
