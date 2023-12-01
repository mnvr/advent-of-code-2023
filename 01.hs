import GHC.IO (catchException, catch)
import Control.Exception (IOException, try)
import System.IO.Error (tryIOError)
import Control.Monad (void)
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = parseLines parse >>= (print . sum)

parseLines :: (String -> a) -> IO [a]
parseLines f = do
    es <- tryIOError getLine
    case es of
        (Left x) -> return []
        (Right s) -> let x = f s in parseLines f >>= (\xs -> return (x : xs))

parse :: String -> Int
parse s = head xs * 10 + last xs
    where xs = map digitToInt $ filter isDigit s
