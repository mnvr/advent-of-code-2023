{-# OPTIONS_GHC -Wno-all #-}

import Data.List

main :: IO ()
-- main = interact $ (++ "\n") . show . p1 . patterns
-- main = interact $ (++ "\n") . p2 . patterns
main = interact $ (++ "\n") . show . p2min . patterns
-- main = interact $ (++ "\n") . p2e . patterns

type Pattern = [String]

patterns :: String -> [Pattern]
patterns = reverse .  last . foldl f ([], []) . lines
  where
    f (p, ps) [] = ([], reverse p : ps)
    f (p, ps) line = (line:p, ps)
    last (p, ps) = if null p then ps else reverse p : ps

p1 :: [Pattern] -> Int
p1 = sum . map rline -- (\p -> (mrow p) * 100 + (mcol p))

-- rline p = let r = (mrow p) in if r /= 0 then r * 100 else (mcol p)
rline p = let c = (mcol p) in if c /= 0 then c else (mrow p) * 100

mcol :: Pattern -> Int
mcol p = go 1 (ncol p)
  where
    ncol (h:_) = length h
    go i n
      | i == n = 0
      | all (uncurry mirror) (map (\line -> splitAt i line) p) = i
      | otherwise = go (i + 1) n

mirror :: Eq a => [a] -> [a] -> Bool
mirror a b = let n = min (length a) (length b) in take n (reverse a) == take n b

mrow :: Pattern -> Int
mrow p = go 1 (length p)
  where
    go i n
      | i == n = 0
      | uncurry mirror (splitAt i p) = i
      | otherwise = go (i + 1) n


-- p2e :: [[String]] -> [Char]
p2e ps = let t2 = (head (drop 1 ps))
 in concatMap (showP t2) ([t2] ++ (variations t2))
 where showP p v = unlines (zipWith (++) (zipWith (++) p (repeat "\t")) v) ++ "\n"


-- p2 :: [Pattern] -> [Int]
p2 ps = concatMap (\(j, p) -> concat $ map (\(i, s, p) -> "variation " ++ show i ++ " of pattern " ++ show j ++ " with sum " ++ show s ++ "\n" ++ unlines p) $ filter (\(_,s,_) -> s > 0) $ map (\(i, p') -> (i, frow p', p')) $ zip [1..] (variations p)) $ zip [1..] ps
-- p2 (p:_) = concat $ map (\(i, s, p) -> "variation " ++ show i ++ " with sum " ++ show s ++ "\n" ++ unlines p) $ filter (\(_,s,_) -> s > 0) $ map (\(i, p') -> (i, frow p', p')) $ zip [0..] (variations p)
--   where
    -- f p = filter (> 0) $ map (\p -> p1 [p]) (variations p)

-- Not sure what the rules are, since we end up with multiple reflections. Here
-- we take the smallest from amongst them.


-- "locate and fix the smudge that causes a different reflection line"
p2min ps = sum $ map g ps
 where
    g p = head $ filter (> 0) $ map (\v -> isDifferentG p v) $ variations p
    isDifferentG p p' =
        let c = (mcol p)
            r = (mrow p)
            c' = (mcol p')
            r' = (mrow p')
        in if c' > 0 && c' /= c then c' else if r' > 0 && r' /= r then r' * 100 else 0

    f p = case filter (> 0) $ map (isDifferent p) $ variations p of
        -- [] -> error (show p)
        [] -> head $ filter (> 0) $ map (rline) $ variations p
        h:_ -> h

    isDifferent p p' =
        let r = rline p
            r' = rline p'
        in if r /= r' then r' else 0
        -- in if (c > 0 && c == c') || (r > 0 && r == r') then 0 else r' * 100 + c'
                    --    in s /= (c * 100 + r)

p2minTooHigh ps = sum $ map f ps
 where
    f p = let vs = variations p in case filter (> 0) (map frow vs) of
        [] -> minimum (filter (> 0) (map fcol vs))
        xs -> minimum xs

p2minTooLow ps = sum $ map f ps
 where
    f p = let vs = variations p in case filter (> 0) (map fcol vs) of
        [] -> minimum (filter (> 0) (map frow vs))
        xs -> minimum xs

fboth p = (fcol p) + (frow p)

frow p = (mrow p) * 100

fcol p = (mcol p)

-- Brute force enumeration
variations :: Pattern -> [Pattern]
variations p = [opposite p y x | y <- [0..length p - 1], x <- [0..length (p !! y) - 1]]
  where
    opposite :: Pattern -> Int -> Int -> Pattern
    opposite p y x = let (up, rest) = splitAt y p
                         ([row], down) = splitAt 1 rest
                         (left, rrow) = splitAt x row
                         ([c], right) = splitAt 1 rrow
                     in up ++ [left ++ ((opp c) : right)] ++ down
    opp :: Char -> Char
    opp '.' = '#'
    opp '#' = '.'
