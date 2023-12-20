import Numeric (readHex)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Step = (Char, Int)

parse :: String -> ([Step], [Step])
parse = unzip . map (line . words) . lines
  where line ([(d:_), c, clr]) = ((d, (read c)), decode clr)
        decode :: String -> (Char, Int)
        decode (_:_:a:b:c:d:e:f:_) = (dir f, hex (a:b:c:d:e:[]))
          where
            dir '0' = 'R'
            dir '1' = 'D'
            dir '2' = 'L'
            dir '3' = 'U'
            hex s = fst $ (\(h:_)->h) (readHex s)

-- Calculate the area of the polygon using:
-- 1. Shoelace's formula to get the area of the interior points,
-- 2. Adding the points on the circumference as we go around it,
-- 3. And adding 1 to account for the origin.
area :: [Step] -> Int
area steps = abs (snd (foldl f ((0,0), 2) steps)) `div` 2
  where
    f ((px, py), s) (d, c) =
        let (x, y) = move (px, py) d c in
            ((x, y), s + c + (py + y) * (px - x))
    move (px, py) 'R' c = (px + c, py)
    move (px, py) 'L' c = (px - c, py)
    move (px, py) 'D' c = (px, py + c)
    move (px, py) 'U' c = (px, py - c)

p1, p2 :: ([Step], [Step]) -> Int
p1 = area . fst
p2 = area . snd
