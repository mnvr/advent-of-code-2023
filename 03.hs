import Data.Char (isDigit)
import Data.Maybe (catMaybes, isJust, fromJust, isNothing)
import Control.Applicative ((<|>), Applicative (liftA2))
import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . (\ps -> (p1 ps, p2 ps)) . parseParts

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }

data Part = Part { partNum :: Int, partNeighbourSymbols :: [Cell] }
data PartDigit = PartDigit { pdChar :: Char, pdNeighbourSymbols :: [Cell] }
data Cell = Cell { cc :: Char, cy :: Int, cx :: Int } deriving (Ord, Eq)

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

neighbouringCells :: Grid -> Int -> Int -> [Cell]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Cell
cell grid y x | isInBounds grid y x =
    Just Cell { cc = (rows grid !! y) !! x, cy = y, cx = x }
  | otherwise = Nothing

isInBounds :: Grid -> Int -> Int -> Bool
isInBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

neighbouringSymbols :: Grid -> Int -> Int -> [Cell]
neighbouringSymbols grid y x = filter isSymbol (neighbouringCells grid y x)

isSymbol :: Cell -> Bool
isSymbol (Cell {cc}) = liftA2 (&&) (not . isDigit) (/= '.') cc

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number are near a symbol.
partDigit :: Grid -> Int -> Int -> Maybe PartDigit
partDigit grid y x = cell grid y x >>= \(Cell {cc}) ->
    case neighbouringSymbolsOfPartDigit_ [] grid y x of
        Just ns -> Just PartDigit { pdChar = cc, pdNeighbourSymbols = ns }
        Nothing -> Nothing

neighbouringSymbolsOfPartDigit_ ::
    [(Int,Int)] -> Grid -> Int -> Int -> Maybe [Cell]
neighbouringSymbolsOfPartDigit_ seen grid y x
    | (y,x) `elem` seen = Nothing
    | not $ isInBounds grid y x = Nothing
    | otherwise =
        let c = rows grid !! y !! x
        in if isDigit c
           then let ns = neighbouringSymbols grid y x
                    seen' = ((y,x) : seen)
                in if null ns
                   then neighbouringSymbolsOfPartDigit_ seen' grid y (x - 1)
                    <|> neighbouringSymbolsOfPartDigit_ seen' grid y (x + 1)
                   else Just ns
           else Nothing

-- Group consecutive sequences of 'Just PartDigit's
splits :: [Maybe PartDigit] -> [[PartDigit]]
splits xs = case span isJust (dropWhile isNothing xs) of
              ([], []) -> []
              (ys, []) -> [map fromJust ys]
              (ys, rest) -> map fromJust ys : splits rest

parts :: Grid -> [Part]
parts = concatMap numbers . partDigitsOrSpaces
  where
    numbers row = map combine (splits row)
    combine pds = Part {
        partNum = read (map pdChar pds),
        partNeighbourSymbols = concatMap pdNeighbourSymbols pds }

partDigitsOrSpaces :: Grid -> [[Maybe PartDigit]]
partDigitsOrSpaces grid = [partDigitsInRow y | y <- [0..my grid]]
  where partDigitsInRow y = [partDigit grid y x | x <- [0..mx grid]]

parseParts :: String -> [Part]
parseParts = parts . makeGrid

p1 :: [Part] -> Int
p1 = sum . map partNum

makeLookupTable :: [Part] -> M.Map Cell [Part]
makeLookupTable ps = modify $ foldl merge M.empty $ zip ps [0..]
  where modify :: M.Map Cell [Int] -> M.Map Cell [Part]
        modify = M.map (map (ps !!))
        merge :: M.Map Cell [Int] -> (Part, Int) -> M.Map Cell [Int]
        merge m (part, i) = foldl (add i) m (partNeighbourSymbols part)
        add :: Int -> M.Map Cell [Int] -> Cell -> M.Map Cell [Int]
        add i m symbol@(Cell {cc = '*'}) = case M.lookup symbol m of
            Nothing -> M.insert symbol [i] m
            Just is -> if i `elem` is then m
                       else M.insert symbol (i:is) m
        add i m _ = m

gearRatio :: [Part] -> Int
gearRatio [x] = 0
gearRatio [x, y] = partNum x * partNum y

p2 :: [Part] -> Int
p2 ps = foldl combine 0 lookupTable
  where lookupTable = makeLookupTable ps
        combine v ps = v + gearRatio ps
