import GHC.IO (catchException, catch)
import Control.Exception (IOException, try)
import System.IO.Error (tryIOError)
import Control.Monad (void)
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = void $ tryIOError processLines

processLines :: IO ()
processLines = processLine >> processLines

processLine :: IO ()
processLine = getLine >>= (print . show . convert)

-- convert :: String -> Integer
convert s = map digitToInt $ filter isDigit s
