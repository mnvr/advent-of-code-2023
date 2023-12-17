import Data.Char (digitToInt)

main :: IO ()
main = interact $ (++ "\n") . show . id . parse

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

p1 = id
