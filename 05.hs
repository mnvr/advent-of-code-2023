import Text.Parsec
import Control.Monad (void)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [RangeMap] }
type RangeMap = [RangeMapping]
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
     rangeMap = mapHeader *> newline *> (rangeMapping `endBy` endOfLineOrFile)
     maps = rangeMap `endBy` endOfLineOrFile
     almanac = Almanac <$> seeds <*> maps
     mkRangeMapping a b c = RangeMapping (Range b c) (Range a c)

-- Guide a seed through the transformations under the given range maps
mTransform :: Int -> [RangeMap] -> Int
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

p1 :: Almanac -> Int
p1 Almanac { seeds, maps } = minimum $ map (`mTransform` maps) seeds

p2 al = 0 -- p1 $ al { seeds = expand (seeds al) }

-- expand :: [Int] -> [Int]
-- expand [] = []
-- expand (x:y:zs) = concat [[x..(x+y)], expand zs]
