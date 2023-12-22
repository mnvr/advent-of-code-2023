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

-- We use two related but separate results here: Shoelace formula, and Pick's
-- theorem.
--
-- The Shoelace formula gives us an easy way of calculating the area† of a
-- polygon with integral coordinates by doing a signed sum of the various
-- trapezoids. There are multiple ways to express the formula, the one we use
-- here computes the signed value
--
--     1/2 * (y1 + y2) * (x1 - x2)
--
-- for each pair of vertices as we go along the polygon. Summed together, they
-- give us the area†.
--
-- But I'm adding a disclaimer '†' to the word area, because just like with
-- Mario, the princess is in another castle, and this this isn't the area we
-- need. The area† given by the Shoelace formula does not account for the width
-- of the boundary cells. Thus, it is neither:
--
-- 1. The area of the entire polygon (which is what we want - the number of
--    integral points on or inside the boundary defined by the polygon's edges).
--
-- 2. Nor is it the internal area (the number of integral points strictly inside
--    the polygon).
--
-- Instead, it is something in between. This is best understood visually. Here
-- is a 3x3 grid, with the dots indicating the centers of the cells:
--
--     +---+---+---+
--     | · | · | · |
--     +---+---+---+
--     | · | · | · |
--     +---+---+---+
--     | · | · | · |
--     +---+---+---+
--
-- The area given by the Shoelace formula would be with respect to these
-- centers, i.e. it'll be the area shown in the enclosed polygon below:
--
--     +---+---+---+
--     | ·---·---· |
--     +-|-+---+-|-+
--     | · | · | · |
--     +-|-+---+-|-+
--     | ·---·---· |
--     +---+---+---+
--
-- Visually, we can see that the area we want is 9 - there are 9 cells on or
-- inside the polygon. But the enclosed polygon covers fractional cells, so we
-- end up with:
--
--    1 (fully internal cell)
--    + 0.5 * 4 (boundary cells, of which only half is covered)
--    + 0.25 * 4 (corner boundary cells) => 1 + 2 + 1 => 4
--
-- Indeed, the value obtained by using Shoelace formula will give us the result
-- 4, and not the 9 that we want.
--
-- So how do we fix this? That's where Pick's theorem enters into the picture.
-- Pick's theorem states that the following quantities:
--
-- - A: The area given by Shoelace formula
-- - I: The number of cells internal to the polygon ("internal area")
-- - B: The number of boundary cells
--
-- Are related by the following equation:
--
--     A = I + B/2 - 1
--
-- Rearranging, we get,
--
--     I = A - B/2 + 1                -- Eq 1
--
-- In our case, what we want is the total area, which is the number of cells
-- inside the polygon (I) plus the number of cells on the boundary (B).
--
-- That is, we want I + B.
--
-- What we already have is A, from our Shoelace computation, and B, the number
-- of perimeter points. So the final value we want is
--
--     I + B
--     = (A - B/2 + 1) + B           (Substituting Eq 1)
--     = A + B/2 + 1
--
-- And that's it. One minor detail is that instead of doing the 1/2 as mentioned
-- in the trapezoid formulation of the Shoelace formula, we just don't do it, so
-- the quantity we end up is actually 2A. Then, when computing the final result,
-- we can divide by 2 only once. In the formulation below, to make it even
-- simpler, we start with 2 instead of 1, so the final compution just becomes
--
--     (2A + B + 2) / 2
--
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
