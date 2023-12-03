import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad (filterM)
import Debug.Trace (trace)

main :: IO ()
main = interact $ (++"\n") . show . parse

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }
  deriving Show

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = take 20 . lines $ s

neighbouringCells :: Grid -> Int -> Int -> [Char]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Char
cell grid y x
  | inBounds grid y x = Just ((rows grid !! y) !! x)
  | otherwise = Nothing

inBounds :: Grid -> Int -> Int -> Bool
inBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

nearSymbol :: Grid -> Int -> Int -> Bool
nearSymbol grid y x = any isSymbol (neighbouringCells grid y x)

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && c /= '.'

-- A particular index is a digit of a part number it is a digit, and if any of
-- the digits of that number are near a symbol.
digitOfPart :: Grid -> Int -> Int -> Maybe Char
digitOfPart grid y x = cell grid y x >>= valid
  where valid d | isDigit d && check [] y x = Just d
                | otherwise = Nothing
        check :: [(Int,Int)] -> Int -> Int -> Bool
        check seen y x
          | (y,x) `elem` seen = False
          | nearSymbol grid y x = True
          | inBounds grid y x =
            let seen' = (y,x) : seen
            in check seen' y (x - 1) || check seen' y (x + 1)
          | otherwise = False

validDigits :: Grid -> [Int]
validDigits = concatMap numbers . validDigitsOrSpaces
  where numbers row = map read (words row)

validDigitsOrSpaces :: Grid -> [String]
validDigitsOrSpaces grid = [partDigitsInRow y | y <- [0..my grid]]
  where partDigitsInRow y = [partDigitOrSpace y x | x <- [0..mx grid]]
        partDigitOrSpace y x = fromMaybe ' ' $ digitOfPart grid y x

parse :: [Char] -> [Int]
parse s = partNumbers
  where grid = makeGrid s
        partNumbers = validDigits grid
