import Control.Applicative (asum, liftA2)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust, fromJust, isNothing)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) p1 p2 . parseParts

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }

data Part = Part { partNum :: Int, pNeighbourSymbols :: [Cell] }
data PartDigit = PartDigit { pdChar :: Char, pdNeighbourSymbols :: [Cell] }
data Cell = Cell { cc :: Char, cy :: Int, cx :: Int } deriving (Ord, Eq)

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

neighbourCells :: Grid -> Int -> Int -> [Cell]
neighbourCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Cell
cell grid y x | isInBounds grid y x =
    Just Cell { cc = (rows grid !! y) !! x, cy = y, cx = x }
  | otherwise = Nothing

isInBounds :: Grid -> Int -> Int -> Bool
isInBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

neighbourSymbols :: Grid -> Int -> Int -> [Cell]
neighbourSymbols grid y x = filter isSymbol (neighbourCells grid y x)

isSymbol :: Cell -> Bool
isSymbol (Cell {cc}) = (not . isDigit) cc && (cc /= '.')

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number is near a symbol.
partDigit :: Grid -> Int -> Int -> Maybe PartDigit
partDigit grid y x = cell grid y x >>= \(Cell {cc}) ->
   PartDigit cc <$> partDigitNeighbourSymbols [] grid y x

partDigitNeighbourSymbols :: [(Int,Int)] -> Grid -> Int -> Int -> Maybe [Cell]
partDigitNeighbourSymbols seen grid y x
    | (y,x) `elem` seen = Nothing
    | not $ isInBounds grid y x = Nothing
    | isDigit (rows grid !! y !! x) =
        let ns = neighbourSymbols grid y x
            seen' = (y,x) : seen
        in asum [guard (not (null ns)) >> Just ns,
                 partDigitNeighbourSymbols seen' grid y (x - 1),
                 partDigitNeighbourSymbols seen' grid y (x + 1)]
    | otherwise = Nothing

parseParts :: String -> [Part]
parseParts = parts . makeGrid

parts :: Grid -> [Part]
parts = concatMap parts_ . maybePartDigits
  where parts_ row = map part (splits row)
        part digits = Part {
            partNum = read (map pdChar digits),
            pNeighbourSymbols = concatMap pdNeighbourSymbols digits }

maybePartDigits :: Grid -> [[Maybe PartDigit]]
maybePartDigits grid =
    [[partDigit grid y x | x <- [0..mx grid]] | y <- [0..my grid]]

-- Group consecutive sequences of 'Just PartDigit's
splits :: [Maybe PartDigit] -> [[PartDigit]]
splits ms = case span isJust (dropWhile isNothing ms) of
              ([], []) -> []
              (js, rest) -> map fromJust js : splits rest

p1 :: [Part] -> Int
p1 = sum . map partNum

makeLookupTable :: [Part] -> M.Map Cell [Part]
makeLookupTable = M.map (map snd) . snd . foldl merge (0, M.empty)
  where merge (i, m') part = (i + 1, foldl add m' (pNeighbourSymbols part))
          where add m symbol@(Cell {cc = '*'}) = case M.lookup symbol m of
                    Nothing -> M.insert symbol [(i, part)] m
                    Just ips -> if isJust (lookup i ips) then m
                                else M.insert symbol ((i,part):ips) m
                add m _ = m

gearRatio :: [Part] -> Int
gearRatio [_] = 0
gearRatio [x, y] = partNum x * partNum y

p2 :: [Part] -> Int
p2 = M.foldl (\s xs -> s + gearRatio xs) 0 . makeLookupTable
