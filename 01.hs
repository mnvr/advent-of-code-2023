import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, findIndex)

main :: IO ()
main = interact $ (++ "\n") . show . (\z -> (p1 z, p2 z)) . lines
  where p1 = sum . map parse1
        p2 = sum . map parse2

parse1 :: String -> Int
parse1 s = let z@(h:_) = xs in h * 10 + last z
    where xs = map digitToInt $ filter isDigit s

parse2 :: String -> Int
parse2 s = first s spelled * 10 + first (reverse s) (map reverse spelled)

first :: String -> [String] -> Int
first s@(c:s') ms = if isDigit c then read [c] else
    case findIndex (`isPrefixOf` s) ms of
        Nothing -> first s' ms
        (Just i) -> i + 1

spelled :: [String]
spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
