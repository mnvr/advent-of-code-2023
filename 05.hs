import Text.Parsec
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [Map] }
type Map = [RangeMapping]
data RangeMapping = RangeMapping { from :: Range, to :: Range }
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
     rangeMapping = mkRangeMapping <$> (num <* sp) <*> (num <* sp) <*> num
     map = mapHeader *> newline *> (rangeMapping `endBy` endOfLineOrFile)
     maps = map `endBy` endOfLineOrFile
     almanac = Almanac <$> seeds <*> maps
     mkRangeMapping a b c = RangeMapping (Range b c) (Range a c)

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
transformRange r (rm:rms) = concatMap transform (intersections r (from rm))
  where transform x = case mapRange rm x of
          Nothing -> transformRange x rms
          Just y -> [y]

-- Not necessarily symmetric.
intersections :: Range -> Range -> [Range]
intersections r@Range { start = s, len = n } r'@Range { start = s', len = n' }
  | s > e' = [r]
  | e < s' = [r]
  | s < s' = mk s (s' - 1) : if e <= e' then [mk s' e] else [mk s' e', mk (e' + 1) e]
  | s <= e' = if e <= e' then [mk s e] else [mk s e', mk (e' + 1) e]
  where e = s + n
        e' = s' + n'
        mk rs re = Range rs (re - rs)

-- This is guaranteed to be called with a range that does not cross over the
-- boundaries of the 'from' range mapping (i.e. either it falls completely
-- within, or is completely outside).
mapRange :: RangeMapping -> Range -> Maybe Range
mapRange RangeMapping { from, to } r@Range { start = s, len = n }
  | inRange from s = Just $ Range (s - start from + start to) n
  | otherwise = Nothing

inRange :: Range -> Int -> Bool
inRange r i = i >= start r && i <= (start r + len r)
