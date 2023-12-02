{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between, newline)
import Text.Parsec.String (Parser)
import Control.Exception (throw)

main :: IO ()
main = interact $ (++"\n") . show . (\gs -> (p1 gs, p2 gs)) . parseGames

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

parseGames :: String -> [Game]
parseGames s = case parse parser "" s of
    Left err -> throw (userError (show err))
    Right g -> g
  where
    parser = game `sepBy` newline <* eof
    int = read <$> many1 digit
    game = do
      i <- between (string' "Game" >> space) (char ':') int
      ds <- draws
      return Game { gid = i, draws = ds }
    draws = draw `sepBy` char ';'
    draw = foldl (<>) mempty <$> (count `sepBy` char ',')
    count = between space space int >>= \i ->
      mempty { red = i } <$ string' "red" <|>
      mempty { green = i } <$ string' "green" <|>
      mempty { blue = i } <$ string' "blue"
