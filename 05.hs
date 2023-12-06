import Text.Parsec
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [Map] }
type Map = [RangeMapping]
data RangeMapping = RangeMapping { from :: Int, to :: Int, rmLen :: Int }
data Range = Range { start :: Int, len :: Int }

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
     rangeMapping = flip RangeMapping <$> (num <* sp) <*> (num <* sp) <*> num
     map = mapHeader *> newline *> (rangeMapping `endBy` endOfLineOrFile)
     maps = map `endBy` endOfLineOrFile
     almanac = Almanac <$> seeds <*> maps

p1, p2 :: Almanac -> Int
p1 Almanac { seeds, maps } = solve (map (`Range` 1) seeds) maps
p2 Almanac { seeds, maps } = solve (seedRanges seeds) maps

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (x:y:rest) = Range x y : seedRanges rest

solve :: [Range] -> [Map] -> Int
solve rs maps = minimum . map start $ foldl transformRanges rs maps

transformRanges :: [Range] -> Map -> [Range]
transformRanges rs m = concatMap (`transformRange` m) rs

-- Transform a seed range under the given range mappings. Such a transformation
-- may cause the range to split.
transformRange :: Range -> [RangeMapping] -> [Range]
transformRange r [] = [r]
transformRange r (rm:rms) = concatMap transform (intersections r rm)
  where transform x | within rm (start x) = [apply rm x]
                    | otherwise = transformRange x rms
        within RangeMapping { from, rmLen = n } i = i >= from && i <= from + n
        apply RangeMapping { from, to } r = Range (start r - from + to) (len r)

-- Not necessarily symmetric.
intersections :: Range -> RangeMapping -> [Range]
intersections r@Range { start = s, len } RangeMapping { from = s', rmLen }
  | s > e' = [r]
  | e < s' = [r]
  | s < s' = mk s (s' - 1) : if e <= e' then [mk s' e] else [mk s' e', mk (e' + 1) e]
  | s <= e' = if e <= e' then [mk s e] else [mk s e', mk (e' + 1) e]
  where e = s + len
        e' = s' + rmLen
        mk rs re = Range rs (re - rs)
