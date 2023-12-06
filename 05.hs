{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
import Text.Parsec
import Control.Monad (void)
import Debug.Trace (trace)

main :: IO ()
-- main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseAlmanac
main = interact $ (++ "\n") . show . p2Debug . parseAlmanac

data Almanac = Almanac { seeds :: [Int], maps :: [Map] } deriving Show
type Map = [RangeMapping]
data RangeMapping = RangeMapping { from :: Range, to :: Range } deriving Show
data Range = Range { start :: Int, len :: Int } deriving Show

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

p2Debug Almanac { seeds, maps } = (,) <$> length <*> id $ z
  where sr' = (seedRanges seeds)
        r = Range 82 1
        sr = [r]
        z = foldl transformRanges sr' maps
        z2 = map (\rm -> mapRange rm r) (head maps)

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (x:y:rest) = Range x y : seedRanges rest

-- Transform seed ranges under the given range map. This may result in more
-- ranges than we started with.
transformRanges :: [Range] -> Map -> [Range]
transformRanges rs m = concatMap (`transformRange` m) rs

-- Transform a seed range under the given range mappings. Such a transformation
-- may cause the range to split.
transformRange :: Range -> [RangeMapping] -> [Range]
transformRange r [] = [r]
transformRange r (rm:rms) = concatMap transform (intersections r (from rm))
  where transform x = trace ("transforming " ++ show x ++ " under " ++ show rm) $ case mapRange rm x of
          Nothing -> transformRange x rms
          Just y -> [trace ("transformed to " ++ show y) y]

-- Not necessarily symmetric.
intersections :: Range -> Range -> [Range]
intersections r@Range { start = s, len = n } r'@Range { start = s', len = n' }
  | s > e' = [r]
  | e < s' = [r]
  | s < s' = [mk s (s' - 1), mk s' e] ++ if e <= e' then [] else [mk (e + 1) e']
  | s <= e' = if e <= e' then [mk s e] else [mk s e', mk (e' + 1) e]
  where e = s + n
        e' = s' + n'
        mk rs re = Range rs (re - rs)

-- This is guaranteed to be called with a range that does not cross over the
-- boundaries of the 'from' range mapping (i.e. either it falls completely
-- within, or is completely outside).
mapRange :: RangeMapping -> Range -> Maybe Range
mapRange RangeMapping { from, to } r@Range { start = s, len = n }
  | s >= start from && s <= (start from + len from) = Just $ Range (s - start from + start to) n
  | otherwise = Nothing

p2Brute :: Almanac -> Int
p2Brute a = 0 -- p1 $ a { seeds = expand (seeds a) }

expand :: [Int] -> [Int]
expand [] = []
expand (x:y:zs) = [x..(x+y)] ++ expand zs
