{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe)

main :: IO ()
main = interact $ (++"\n") . show . sum . parse

data Grid = Grid { rows :: [String], my :: Int, mx :: Int }
  deriving Show

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, my = length ls - 1, mx = length (head ls) - 1 }
    where ls = take 20 . drop 0 . lines $ s

neighbouringCells :: Grid -> Int -> Int -> [Char]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1],
                                 not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Char
cell grid y x
  | isInBounds grid y x = Just ((rows grid !! y) !! x)
  | otherwise = Nothing

isInBounds :: Grid -> Int -> Int -> Bool
isInBounds (Grid {my, mx}) y x = y >= 0 && y <= my && x >= 0 && x <= mx

nearSymbol :: Grid -> Int -> Int -> Bool
nearSymbol grid y x = any isSymbol (neighbouringCells grid y x)

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && c /= '.'

-- A particular index is a digit of a part number if (a) it is a digit, and (b)
-- if any of the digits of that number are near a symbol.
digitOfPart :: Grid -> Int -> Int -> Maybe Char
digitOfPart grid y x = cell grid y x >>= \c ->
    if digitOfPart_ [] grid y x then Just c else Nothing

digitOfPart_ :: [(Int,Int)] -> Grid -> Int -> Int -> Bool
digitOfPart_ seen grid y x
    | (y,x) `elem` seen = False
    | not $ isInBounds grid y x = False
    | otherwise =
        let c = rows grid !! y !! x
        in if not (isDigit c) then False
           else nearSymbol grid y x
                || digitOfPart_ ((y,x) : seen) grid y (x - 1)
                || digitOfPart_ ((y,x) : seen) grid y (x + 1)

validDigits :: Grid -> [Int]
validDigits = concatMap numbers . validDigitsOrSpaces
  where numbers row = map read (words row)

validDigitsOrSpaces :: Grid -> [String]
validDigitsOrSpaces grid = [partDigitsInRow y | y <- [0..my grid]]
  where partDigitsInRow y = [partDigitOrSpace y x | x <- [0..mx grid]]
        partDigitOrSpace y x = fromMaybe ' ' $ digitOfPart grid y x

-- parse :: [Char] -> [Int]
parse s = partNumbers
  where grid = makeGrid s
        partNumbers = validDigits grid
        partNumbers2 = nearSymbol grid 0 9
