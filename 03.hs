import Data.Char (isDigit)
import Data.Maybe (catMaybes)

main :: IO ()
main = interact $ (++"\n") . show . parse

data Grid = Grid { rows :: [String], rowCount :: Int, colCount :: Int }
  deriving Show

makeGrid :: String -> Grid
makeGrid s = Grid { rows = ls, rowCount = length ls, colCount = length (head ls) }
    where ls = take 4 . drop 0 . lines $ s

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && c /= '.'

neighbouringCells :: Grid -> Int -> Int -> [Char]
neighbouringCells grid y x = catMaybes
    [cell grid (y + u) (x + v) | u <- [-1,0,1], v <- [-1,0,1], not (u == 0 && v == 0)]

cell :: Grid -> Int -> Int -> Maybe Char
cell (Grid {rows, rowCount, colCount}) y x
  | y >= 0 && y < rowCount && x >= 0 && x < colCount = Just ((rows !! y) !! x)
  | otherwise = Nothing

parse s = test
  where grid = makeGrid s
        test = neighbouringCells grid 0 2
