-- An ELI5-ish (for 5 year old Haskell programmers, though if they are five year
-- old and programming in Haskell, they might be able to explain me a thing or
-- two) explanation of the most interesting, and the scariest, part of the
-- solution to Day 5 Part 2 - calculating the splits.

type Range = (Int, Int)

splits :: Range -> Range -> [Range]
splits (s, e) (s', e')
  | s > e' = [(s, e)]
  | e < s' = [(s, e)]
  | s < s' = (s, s' - 1) : if e <= e' then [(s', e)] else [(s', e'), (e' + 1, e)]
  | s <= e' = if e <= e' then [(s, e)] else [(s, e'), (e' + 1, e)]

main :: IO ()
main = do
    print $ splits (1, 9) (10, 20)
    print $ splits (1, 9) (7, 20)
    print $ splits (1, 7) (7, 20)
