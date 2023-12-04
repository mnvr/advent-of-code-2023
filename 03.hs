import Control.Applicative (asum, liftA2)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (catMaybes, isJust, fromJust, isNothing)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) p1 p2 . parseParts

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }

data Part = Part { partNum :: Int, partSymbols :: [Cell] } deriving (Ord, Eq)
data PartDigit = PartDigit { pdChar :: Char, pdSymbols :: [Cell] }
data Cell = Cell { cc :: Char, cy :: Int, cx :: Int } deriving (Ord, Eq)

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

region :: Grid -> Int -> Int -> [Cell]
region grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Cell
cell grid y x | outOfBounds grid y x = Nothing
  | otherwise =  Just Cell { cc = rows grid !! y !! x, cy = y, cx = x }

outOfBounds :: Grid -> Int -> Int -> Bool
outOfBounds (Grid {my, mx}) y x = y < 0 || y > my || x < 0 || x > mx

regionSymbols :: Grid -> Int -> Int -> [Cell]
regionSymbols grid y x = filter isSymbol (region grid y x)

isSymbol :: Cell -> Bool
isSymbol (Cell {cc}) = (not . isDigit) cc && (cc /= '.')

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number is near a symbol.
maybePartDigit :: Grid -> Int -> Int -> Maybe PartDigit
maybePartDigit grid y x = cell grid y x >>= \(Cell {cc}) ->
   PartDigit cc <$> findPartDigitSymbols [] grid y x

findPartDigitSymbols :: [(Int,Int)] -> Grid -> Int -> Int -> Maybe [Cell]
findPartDigitSymbols seen grid y x
    | (y,x) `elem` seen = Nothing
    | outOfBounds grid y x = Nothing
    | isDigit (rows grid !! y !! x) =
        let ns = regionSymbols grid y x
            seen' = (y,x) : seen
        in asum [guard (not (null ns)) >> Just ns,
                 findPartDigitSymbols seen' grid y (x - 1),
                 findPartDigitSymbols seen' grid y (x + 1)]
    | otherwise = Nothing

parseParts :: String -> [Part]
parseParts = parts . makeGrid

parts :: Grid -> [Part]
parts = concatMap parts_ . maybePartDigits
  where parts_ row = map part (splits row)
        part digits = Part { partNum = read (map pdChar digits),
                             partSymbols = concatMap pdSymbols digits }

maybePartDigits :: Grid -> [[Maybe PartDigit]]
maybePartDigits grid =
    [[maybePartDigit grid y x | x <- [0..mx grid]] | y <- [0..my grid]]

-- Group consecutive runs of 'Just PartDigit's
splits :: [Maybe PartDigit] -> [[PartDigit]]
splits ms = case span isJust (dropWhile isNothing ms) of
              ([], []) -> []
              (js, rest) -> map fromJust js : splits rest

p1 :: [Part] -> Int
p1 = sum . map partNum

makeGearIndex :: [Part] -> M.Map Cell (S.Set (Int, Part))
makeGearIndex = snd . foldl f1 (0, M.empty)
  where f1 (i, m') part = (i + 1, foldl f2 m' (partSymbols part))
          where f2 m symbol@(Cell {cc = '*'}) = M.insert symbol ys m
                  where ys = case M.lookup symbol m of
                               Nothing -> S.singleton (i, part)
                               Just s -> S.insert (i, part) s
                f2 m _ = m

makeGearIndex2 :: [Part] -> M.Map Cell [Part]
makeGearIndex2 parts = m3
  where m1 :: M.Map Cell (S.Set (Int, Part))
        m1 = makeGearIndex parts
        m2 :: M.Map Cell [(Int, Part)]
        m2 = M.map S.elems m1
        m3 :: M.Map Cell [Part]
        m3 = M.map (\e -> map (\(i, p) -> p) e) m2

gearRatio :: [Part] -> Int
gearRatio [_] = 0
gearRatio [x, y] = partNum x * partNum y

p2 :: [Part] -> Int
p2 = M.foldl (\s xs -> s + gearRatio xs) 0 . makeGearIndex2
