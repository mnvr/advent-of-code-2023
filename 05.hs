import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseAlmanac

data Almanac = Input { seeds :: [Int], maps :: [Map] }
data Map = Map { sourceRange :: Int, destinationRange :: Int, length :: Int } deriving Show

-- parseAlmanac :: String -> Input
parseAlmanac s = case parse almanac "" s of
    Left err -> error (show err)
    Right v -> v
  where
     sp :: Parser Char = char ' '
     num :: Parser Int
     num = read <$> many1 digit
     nums :: Parser [Int]
     nums = sepBy num sp
     mapHeader :: Parser Char
     mapHeader = many1 (letter <|> char '-' <|> char ' ') >> char ':'
     endOfLineOrFile :: Parser ()
     endOfLineOrFile = void endOfLine <|> eof
     range :: Parser [Int]
     range = do
        n1 <- num
        sp
        n2 <- num
        sp
        n3 <- num
        pure [n1, n2, n3]
     ranges = do
        many1 (range >>= \ns -> endOfLineOrFile >> pure ns)
     map = do
        mapHeader
        newline
        ranges
     maps = do
        many1 (map >>= \m -> endOfLineOrFile >> pure m)
     almanac = do
        string "seeds: "
        seeds <- nums
        newline
        newline
        m <- maps
        pure (seeds, m)

p1 = id
