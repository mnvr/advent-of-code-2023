import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Control.Applicative (liftA2)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) p1 p2 . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [RangesMap] } deriving Show
type RangesMap = [RangeMap]
data RangeMap = RangeMap { destinationRange :: Int, sourceRange :: Int, rangeLength :: Int } deriving Show

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
        pure $ Almanac seeds (fmap convRangesMap m)
     convRangesMap :: [[Int]] -> RangesMap
     convRangeMap :: [Int] -> RangeMap
     convRangeMap [a, b, c] = RangeMap a b c
     convRangesMap = fmap convRangeMap


-- Guide a seed through the maps
rtraverse :: [RangesMap] -> Int -> Int
rtraverse [] s = s
rtraverse (m:ms) s = rtraverse ms (mapRangesMap m s)

mapRangesMap :: RangesMap -> Int -> Int
mapRangesMap [] s = s
mapRangesMap (rm:rms) s = case mapRangeMap rm s of
    Just s -> s
    Nothing -> mapRangesMap rms s

mapRangeMap :: RangeMap -> Int -> Maybe Int
mapRangeMap RangeMap { destinationRange, sourceRange, rangeLength } s =
    if s >= sourceRange && s <= (sourceRange + rangeLength) then Just (destinationRange + s - sourceRange) else Nothing

p1 Almanac { seeds, maps } = xsmin $ fmap (rtraverse maps) seeds

xsmin xs = foldl1 min xs

p2 al = p1 $ al { seeds = expand (seeds al) }

expand :: [Int] -> [Int]
expand [] = []
expand (x:y:zs) = concat [[x..(x+y)], expand zs]
