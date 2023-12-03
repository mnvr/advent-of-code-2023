import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Applicative ((<|>))

main :: IO ()
main = interact $ (++ "\n") . show . p1

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }
  deriving Show

data Part = Part { pc :: Char, symbols :: [Cell] }
data Cell = Cell { c :: Char, y :: Int, x :: Int }

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = lines s

neighbouringCells :: Grid -> Int -> Int -> [Cell]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Cell
cell grid y x
  | isInBounds grid y x = Just Cell { c = (rows grid !! y) !! x, y = y, x = x }
  | otherwise = Nothing

isInBounds :: Grid -> Int -> Int -> Bool
isInBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

neighbouringSymbols :: Grid -> Int -> Int -> [Cell]
neighbouringSymbols grid y x = filter isSymbol (neighbouringCells grid y x)

nearSymbol :: Grid -> Int -> Int -> Bool
nearSymbol grid y x = not . null $ neighbouringSymbols grid y x

isSymbol :: Cell -> Bool
isSymbol (Cell {c}) = (not . isDigit) c && c /= '.'

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number are near a symbol.
digitOfPart :: Grid -> Int -> Int -> Maybe Char
digitOfPart grid y x = cell grid y x >>= \(Cell {c}) ->
    c <$ digitOfPart_ [] grid y x

digitOfPart_ :: [(Int,Int)] -> Grid -> Int -> Int -> Maybe [Cell]
digitOfPart_ seen grid y x
    | (y,x) `elem` seen = Nothing
    | not $ isInBounds grid y x = Nothing
    | otherwise =
        let c = rows grid !! y !! x
        in if isDigit c
           then let ns = neighbouringSymbols grid y x
                in if null ns
                   then digitOfPart_ ((y,x) : seen) grid y (x - 1)
                    <|> digitOfPart_ ((y,x) : seen) grid y (x + 1)
                   else Just ns
           else Nothing

validDigits :: Grid -> [Int]
validDigits = concatMap numbers . validDigitsOrSpaces
  where numbers row = map read (words row)

validDigitsOrSpaces :: Grid -> [String]
validDigitsOrSpaces grid = [partDigitsInRow y | y <- [0..my grid]]
  where partDigitsInRow y = [partDigitOrSpace y x | x <- [0..mx grid]]
        partDigitOrSpace y x = fromMaybe ' ' $ digitOfPart grid y x

partNumbers :: String -> [Int]
partNumbers = validDigits . makeGrid

p1 :: String -> Int
p1 = sum . partNumbers
