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
data Op = Remove | Replace Lens deriving Show
type Step = (Op, Int)

decode :: String -> Step
decode s = case break (`elem` "-=") s of
    (a, '-':b) -> (Remove, hash a)
    (a, '=':b) -> (Replace (a, read b), hash a)

-- p2 :: [String] -> Int
-- p2 = foldl f boxes
-- p2 :: [String] -> [[Int]]
-- p2 :: [String] -> [[Int]]
p2 = map decode
-- p2 = foldl f boxes . map decode
--   where
--     boxes = take 256 $ repeat []
--     f bs = (`modify` bs)
--     modify st = zipWith (modifyBox st) [1..]
--     modifyBox st = zipWith ( st)
