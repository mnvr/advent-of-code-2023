import Data.Text qualified as T

-- Variation of 02.hs that does manual line by line parsing using Data.Text
-- instead of using Parsec.

main :: IO ()
main = interact $ (++"\n") . show . (\gs -> (p1 gs, p2 gs)) . map parseGame . lines
-- main = debug1

debug1 :: IO ()
debug1 = interact $  unlines . concatMap each . take 10 . lines
  where each l = let g = parseGame l in
          [l, show g, show . length $ filter possible [g]]

debug2 :: IO ()
debug2 = interact $  unlines . concatMap each . take 10 . lines
  where each l = let g = parseGame l in
          [l, show g, show $ fewest (draws g), show . power $ fewest (draws g)]

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

instance Semigroup Draw where
  (Draw a b c) <> (Draw x y z) = Draw (a + x) (b + y) (c + z)

instance Monoid Draw where
  mempty = Draw 0 0 0

possible :: Game -> Bool
possible = all valid . draws
  where valid (Draw r g b) = r <= 12 && g <= 13 && b <= 14

p1 :: [Game] -> Int
p1 = sum . map gid . filter possible

power :: Draw -> Int
power (Draw r g b) = r * g * b

fewest :: [Draw] -> Draw
fewest = foldl m mempty
  where m (Draw a b c) (Draw x y z) = Draw (max a x) (max b y) (max c z)

powers :: [Game] -> [Int]
powers = map (power . fewest . draws)

p2 :: [Game] -> Int
p2 = sum . powers

parseGame :: String -> Game
parseGame s = Game { gid = gid, draws = draws }
  where [gt, rt] = T.split (==':') (T.pack s)
        gid = read . T.unpack . last $ T.words gt
        draws = map draw $ T.split (==';') rt
        draw t = foldl update mempty (T.split (==',') t)
        update d it = case color of
          "red" -> d { red = count }
          "green" -> d { green = count }
          "blue" -> d { blue = count }
          where [count', color'] = T.words it
                color = T.unpack color'
                count = (read . T.unpack) count'
