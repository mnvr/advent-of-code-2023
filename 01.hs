import GHC.IO (catchException, catch)
import Control.Exception (IOException, try)
import System.IO.Error (tryIOError)
import Control.Monad (void)
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
    x <- next
    y <- print (show x)
    return ()

next :: IO [Int]
next = do
    l1 <- tryIOError getLine
    case l1 of
        (Left x) -> return []
        (Right s) -> let x = convert s in next >>= (\xs -> return (x : xs))


convert :: String -> Int
convert s = head xs * 10 + last xs
    where xs = map digitToInt $ filter isDigit s
