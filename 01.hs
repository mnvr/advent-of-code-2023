import GHC.IO (catchException)
import Control.Exception (IOException)

main :: IO ()
main = catchException processLines (\(e :: IOException) -> pure ())

processLines :: IO ()
processLines = processLine >> processLines

processLine :: IO ()
processLine = getLine >>= print
