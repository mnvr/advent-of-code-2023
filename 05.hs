import Text.Parsec
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [RangesMap] } deriving Show
type RangesMap = [RangeMap]
data RangeMap = RangeMap { destinationRange :: Int, sourceRange :: Int, rangeLength :: Int } deriving Show

parseAlmanac :: String -> Almanac
parseAlmanac s = case parse almanac "" s of
    Left err -> error (show err)
    Right v -> v
  where
     sp = char ' '
     num = read <$> many1 digit
     nums = num `sepBy` sp
     seeds = string "seeds: " *> nums <* count 2 newline
     mapHeader = many1 (letter <|> char '-' <|> sp) >> char ':'
     endOfLineOrFile = void endOfLine <|> eof
     range = (,,) <$> (num <* sp) <*> (num <* sp) <*> num
     ranges = range `endBy` endOfLineOrFile
     map = mapHeader *> newline *> ranges
     maps = map `endBy` endOfLineOrFile
     almanac = Almanac <$> seeds <*> (fmap convRangesMap <$> maps)
     convRangesMap :: [(Int,Int,Int)] -> RangesMap
     convRangesMap = fmap convRangeMap
     convRangeMap :: (Int,Int,Int) -> RangeMap
     convRangeMap (a, b, c) = RangeMap a b c


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
