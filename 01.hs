import GHC.IO (catchException, catch)
import Control.Exception (IOException, try)
import System.IO.Error (tryIOError)
import Control.Monad (void)

main :: IO ()
main = void $ tryIOError processLines

processLines :: IO ()
processLines = processLine >> processLines

processLine :: IO ()
processLine = getLine >>= print
