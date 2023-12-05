import Text.Parsec
import Text.Parsec.String (Parser)

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
     range :: Parser [Int]
     range = do
        n1 <- num
        sp
        n2 <- num
        sp
        n3 <- num
        pure [n1, n2, n3]
     _ranges1 = do
        many1 (range >>= \ns -> newline >> pure ns)
     alMap = do
        mapHeader
        newline
        _ranges1
     almanac = do
        string "seeds: "
        seeds <- nums
        newline
        newline
        ranges1 <- alMap
        newline
        ranges2 <- alMap
        newline
        ranges3 <- alMap
        -- newline

        -- newline

        -- range2 <- nums
        -- newline
        -- newline
        -- string "soil-to-fertilizer map:"
        -- newline
        -- range3 <- nums
        -- newline
        -- range4 <- nums
        -- newline
        -- range5 <- nums
        -- newline
        pure (seeds, [ranges1, ranges2, ranges3])


        -- seeds = (,) <$> ( *> nums <* count 2 newline) <*> maps
        -- num :: Parser Int
        -- num = read <$> many1 digit
        -- nums = many1 (between sp sp num) --`sepBy` spaces
        -- maps = (,) <$> mhead <*> range
        -- mhead = manyTill (noneOf "\n") endOfLine
        -- ranges = count 2 range -- range `sepBy` newline
        -- range = count 3 nums
        -- sp = many (char ' ')



p1 = id
