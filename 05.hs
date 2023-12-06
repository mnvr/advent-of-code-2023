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

p1 :: Almanac -> Int
p1 Almanac { seeds, maps } = minimum $ map (`mTransform` maps) seeds

-- Guide a seed through the transformations under the given maps
mTransform :: Int -> [Map] -> Int
mTransform = foldl rmTransform

-- Transform a seed using the given range mappings
rmTransform :: Int -> [RangeMapping] -> Int
rmTransform s [] = s
rmTransform s (rm:rms) = case rmApply rm s of
    Just s -> s
    Nothing -> rmTransform s rms

-- Apply the given range mapping to the seed if it lies in the source range.
rmApply :: RangeMapping -> Int -> Maybe Int
rmApply RangeMapping { from, to } s = case offsetInRange from s of
    Nothing -> Nothing
    Just o -> Just (start to + o)

-- If the given seed falls in the given range, then return its offset from the
-- start of the range.
offsetInRange :: Range -> Int -> Maybe Int
offsetInRange Range { start, len } x =
    if x >= start && x <= (start + len) then Just (x - start) else Nothing

p2 :: Almanac -> Int
p2 Almanac { seeds, maps } = minimum . map start . filter (\r -> len r /= 0) $
    foldl transformRanges (seedRanges seeds) maps

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (x:y:rest) = Range x y : seedRanges rest

-- Transform seed ranges under the given range map. This may result in more
-- ranges than we started with.
transformRanges :: [Range] -> Map -> [Range]
transformRanges rs m = concatMap (`transformRange` m) rs

-- Transform a seed range under the given range map (which is a list of range
-- mappings really). Such a transformation may cause the range to split.
transformRange :: Range -> [RangeMapping] -> [Range]
transformRange r [] = [r]
transformRange r (rm:rms) = case applyRangeMapping rm r of
    (Nothing, remaining) -> concatMap (`transformRange` rms) remaining
    (Just transformed, remaining) -> transformed : concatMap (`transformRange` rms) remaining

-- Apply the given range mapping to the given range. The range mapping may be
-- applicable to a subset of the range, so this will return a (optional)
-- transformed range, and the ranges (possibly empty) that were unchanged.
--
-- Can produce zero length ranges, but that should be fine.
applyRangeMapping :: RangeMapping -> Range -> (Maybe Range, [Range])
applyRangeMapping RangeMapping { from, to } range@(Range { start = p, len = n })
    -- no overlap (left case, right case)
  | q < start from || p > end from = (Nothing, [range])
    -- left overlap only
  | q >= start from && q <= end from =
        (jr (start to) (q - start from), [Range p (start from - p - 1)])
    -- range lies entirely inside from
  | p >= start from && q <= end from =
        (jr (start to + (p - start from)) n, [])
    -- right overlap only
  | p >= start from && q >= end from =
        (jr (start to + (p - start from)) (end from - p),
         [Range (end from + 1) (q - (end from + 1))])
    -- range extends outside from on both ends
  | otherwise =
        (jr (start to) (len from),
         [Range p (start from - p), Range (q + 1) (q - end from - 1)])

  where end Range { start, len } = start + len
        q = end range
        jr s l = Just (Range s l)

p2Brute :: Almanac -> Int
p2Brute a = 0 -- p1 $ a { seeds = expand (seeds a) }

expand :: [Int] -> [Int]
expand [] = []
expand (x:y:zs) = [x..(x+y)] ++ expand zs
