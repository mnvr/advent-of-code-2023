{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

import Data.Text qualified as T

main :: IO ()
main = interact $ (++"\n") . show . (\gs -> (p1 gs, p2 gs)) . map parse . lines
-- main = debug1

debug1 :: IO ()
debug1 = interact $  unlines . concatMap each . take 10 . lines
  where each l = let g = parse l in
          [l, show g, show . length $ filter possible [g]]

debug2 :: IO ()
debug2 = interact $  unlines . concatMap each . take 10 . lines
  where each l = let g = parse l in
          [l, show g, show $ fewest dzero g, show . power $ fewest dzero g]

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

parse :: String -> Game
parse s = Game { gid = gid, draws = draws }
  where [gt, rt] = T.split (==':') (T.pack s)
        gid = read . T.unpack . last $ T.words gt
        draws = map parseDraw $ T.split (==';') rt

parseDraw :: T.Text -> Draw
parseDraw t = foldl update dzero iters
    where iters = T.split (==',') t
          update d it = case color of
            "red" -> d { red = count }
            "green" -> d { green = count }
            "blue" -> d { blue = count }
            where [count', color'] = T.words it
                  color = T.unpack color'
                  count = (read . T.unpack) count'

dzero :: Draw
dzero = Draw { red = 0, green = 0, blue = 0 }

p1Threshold :: Draw
p1Threshold = Draw { red = 12, green = 13, blue = 14 }

belowThreshold :: Draw -> Draw -> Bool
belowThreshold threshold draw = and [red draw <= red threshold,
                                     green draw <= green threshold,
                                     blue draw <= blue threshold]

possible :: Game -> Bool
possible = all (belowThreshold p1Threshold) . draws

p1 :: [Game] -> Int
p1 = sum . map gid . filter possible

power :: Draw -> Int
power Draw { red = r, green = g, blue = b } = r * g * b

fewest :: Draw -> Game -> Draw
fewest d g = foldl m d (draws g)
  where m d1 d2 = Draw { red = max (red d1) (red d2),
                         green = max (green d1) (green d2),
                         blue = max (blue d1) (blue d2) }

powers :: [Game] -> [Int]
powers = map (power . fewest dzero)

p2 :: [Game] -> Int
p2 = sum . powers
