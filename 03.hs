import Control.Applicative (asum)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust, fromJust, isNothing)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseParts

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }

data Part = Part { partNum :: Int, partSymbols :: [Cell] }
data PartDigit = PartDigit { pdChar :: Char, pdSymbols :: [Cell] }
data Cell = Cell { cc :: Char, cy :: Int, cx :: Int } deriving (Ord, Eq)

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

region :: Grid -> Int -> Int -> [Cell]
region grid y x = catMaybes $
  tail [cell grid (y + u) (x + v) | u <- [0,-1,1], v <- [0,-1,1]]

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

makeGearIndex :: [Part] -> M.Map Cell [Part]
makeGearIndex = M.map (map snd) . snd . foldl f1 (0, M.empty)
  where f1 (i, m') part = (i + 1, foldl f2 m' (partSymbols part))
          where f2 m symbol@(Cell {cc = '*'}) = case M.lookup symbol m of
                    Nothing -> M.insert symbol [(i, part)] m
                    Just xs -> if isJust (lookup i xs) then m
                               else M.insert symbol ((i, part) : xs) m
                f2 m _ = m

gearRatio :: [Part] -> Int
gearRatio [_] = 0
gearRatio [x, y] = partNum x * partNum y

p2 :: [Part] -> Int
p2 = M.foldl (\s xs -> s + gearRatio xs) 0 . makeGearIndex
