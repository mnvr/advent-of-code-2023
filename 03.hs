import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Applicative ((<|>))

main :: IO ()
main = interact $ (++ "\n") . show . p1

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }
  deriving Show

data PartDigit = Part { pdChar :: Char, pdNeighbourSymbols :: [Cell] }
data Cell = Cell { cc :: Char, cy :: Int, cx :: Int } deriving Eq

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

neighbouringCells :: Grid -> Int -> Int -> [Cell]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Cell
cell grid y x
  | isInBounds grid y x = Just Cell {
                            cc = (rows grid !! y) !! x, cy = y, cx = x }
  | otherwise = Nothing

isInBounds :: Grid -> Int -> Int -> Bool
isInBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

neighbouringSymbols :: Grid -> Int -> Int -> [Cell]
neighbouringSymbols grid y x = filter isSymbol (neighbouringCells grid y x)

nearSymbol :: Grid -> Int -> Int -> Bool
nearSymbol grid y x = not . null $ neighbouringSymbols grid y x

isSymbol :: Cell -> Bool
isSymbol (Cell {cc}) = (not . isDigit) cc && cc /= '.'

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number are near a symbol.
partDigit :: Grid -> Int -> Int -> Maybe Char
partDigit grid y x = cell grid y x >>= \(Cell {cc}) ->
    cc <$ neighbouringSymbolsOfPartDigit_ [] grid y x

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

validDigits :: Grid -> [Int]
validDigits = concatMap numbers . validDigitsOrSpaces
  where numbers row = map read (words row)

validDigitsOrSpaces :: Grid -> [String]
validDigitsOrSpaces grid = [partDigitsInRow y | y <- [0..my grid]]
  where partDigitsInRow y = [partDigitOrSpace y x | x <- [0..mx grid]]
        partDigitOrSpace y x = fromMaybe ' ' $ partDigit grid y x

partNumbers :: String -> [Int]
partNumbers = validDigits . makeGrid

p1 :: String -> Int
p1 = sum . partNumbers
