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
    where xs = map digitToInt $ filter isDigit s
