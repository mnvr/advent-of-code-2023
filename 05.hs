import Text.Parsec
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2Brute) . parseAlmanac

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
p2 Almanac { seeds, maps } = length $
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
    (Nothing, Nothing) -> error "where did the range disappear!"
    (Nothing, Just unchanged) -> transformRange unchanged rms
    (Just transformed, Nothing) -> [transformed]
    (Just transformed, Just remaining) -> transformed : transformRange remaining rms

-- Apply the given range mapping to the given range. The range mapping may be
-- applicable to a subset of the range, so this will return a (optional)
-- transformed range, and (possibly) the range that was unchanged.
applyRangeMapping :: RangeMapping -> Range -> (Maybe Range, Maybe Range)
applyRangeMapping rm r = undefined

transformRangeUsingMappings s (rm:rms) = case rmApply rm s of
    Just s -> s
    Nothing -> transformRangeUsingMappings s rms

-- Apply the given range mapping to the seed if it lies in the source range.
rmApplyR :: RangeMapping -> Int -> Maybe Int
rmApplyR RangeMapping { from, to } s = case offsetInRangeR from s of
    Nothing -> Nothing
    Just o -> Just (start to + o)

-- If the given seed falls in the given range, then return its offset from the
-- start of the range.
offsetInRangeR :: Range -> Int -> Maybe Int
offsetInRangeR Range { start, len } x =
    if x >= start && x <= (start + len) then Just (x - start) else Nothing

p2Brute al = 0 -- p1 $ al { seeds = expand (seeds al) }

-- expand :: [Int] -> [Int]
-- expand [] = []
-- expand (x:y:zs) = concat [[x..(x+y)], expand zs]
