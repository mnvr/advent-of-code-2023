import Data.Char (isDigit)
import Data.List (isPrefixOf, findIndex)

-- Challenge: Use only 2 variables

main :: IO ()
main = interact $ (++ "\n") . show . sum . map parse . lines

parse :: String -> Int
parse s = read $ [id, reverse] >>= (\f -> first (f s) f)

first :: String -> (String -> String) -> String
first s f = if isDigit (head s) then take 1 s else
    maybe (first (tail s) f) (show . (+1)) (findIndex (`isPrefixOf` s) (map f
             ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]))
