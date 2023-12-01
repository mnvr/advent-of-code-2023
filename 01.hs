import System.IO.Error (tryIOError)
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = parseLines parse >>= (print . sum)

parseLines :: (String -> a) -> IO [a]
parseLines f = tryIOError getLine >>= e
  where e (Left _) = pure []
        e (Right s) = parseLines f >>= (\xs -> pure (f s : xs))

parse :: String -> Int
parse s = head xs * 10 + last xs
    where xs = map digitToInt $ filter isDigit s
