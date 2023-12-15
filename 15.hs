import Data.Char (ord)

main :: IO ()
main = interact $ (++ "\n") . show . p2 . steps . lines

steps :: [String] -> [String]
steps = concatMap f
  where
    f [] = []
    f s = let (a, b) = span (/= ',') s in a : f (drop 1 b)

p1 :: [String] -> Int
p1 = sum . map hash

hash :: String -> Int
hash = foldl (\v c -> (v + (ord c)) * 17 `mod` 256) 0

type Lens = (String, Int)
data Op = Remove String | Replace Lens
type Step = (Op, Int)

decode :: String -> Step
decode s = case break (`elem` "-=") s of
    (a, '-':b) -> (Remove a, hash a)
    (a, '=':b) -> (Replace (a, read b), hash a)

-- p2 :: [String] -> Int
p2 = filter (not . null) . foldl f boxes . map decode
  where
    boxes = take 256 $ repeat []
    f bs = (`modify` bs)
    modify step = zipWith (modifyBox step) [0..]
    modifyBox (op, si) i box
      | si == i = modifyBox' op box
      | otherwise = box
    modifyBox' (Remove label) = filter ((/= label) . fst)
    modifyBox' (Replace lens) = reverse . append . foldl g (Just lens, [])
      where
        g (Nothing, box) lens' = (Nothing, lens' : box)
        g (Just lens, box) lens'
              | (fst lens) == (fst lens') = (Nothing, lens : box)
              | otherwise = (Just lens, lens' : box)
        append (Nothing, box) = box
        append (Just lens, box) = lens : box
