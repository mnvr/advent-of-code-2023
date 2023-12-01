import Data.Char (isDigit)
import Data.List (isPrefixOf, findIndex)

-- Challenge: Use only 2 variables

main :: IO ()
main = interact $ (++ "\n") . show . sum . fmap parse . lines

parse :: String -> Int
parse s = first s id * 10 + first (reverse s) reverse

first :: String -> (String -> String) -> Int
first s f = if isDigit (head s) then read [head s] else
    maybe (first (tail s) f) (+1) (findIndex (`isPrefixOf` s) (map f
             ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]))

