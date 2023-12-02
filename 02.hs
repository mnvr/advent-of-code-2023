{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

import Data.Text qualified as T

main :: IO ()
main = interact $ (++"\n") . show . sum . map gid . p1 . map parse . lines

debug :: IO ()
debug = interact $  unlines . concatMap each . take 10 . lines
  where each l = let g = parse l in [l, show g, show $ length $ p1 [g]]

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

parse :: String -> Game
parse s = Game { gid = gid, draws = draws }
  where [gt, rt] = T.split (==':') (T.pack s)
        gid = read . T.unpack . last $ T.words gt
        draws = map parseDraw $ T.split (==';') rt

parseDraw :: T.Text -> Draw
parseDraw t = foldl update zero iters
    where zero = Draw { red = 0, green = 0, blue = 0 }
          iters = T.split (==',') t
          update d it = case color of
            "red" -> d { red = count }
            "green" -> d { green = count }
            "blue" -> d { blue = count }
            where [count', color'] = T.words it
                  color = T.unpack color'
                  count = (read . T.unpack) count'

p1Threshold :: Draw
p1Threshold = Draw { red = 12, green = 13, blue = 14 }

belowThreshold :: Draw -> Draw -> Bool
belowThreshold threshold draw = and [red draw <= red threshold,
                                     green draw <= green threshold,
                                     blue draw <= blue threshold]

possible :: Game -> Bool
possible = all (belowThreshold p1Threshold) . draws

p1 :: [Game] -> [Game]
p1 = filter possible
