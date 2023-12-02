{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between, newline)
import Text.Parsec.String (Parser)
import Control.Exception (throw)

main :: IO ()
main = interact $ (++"\n") . show . (\gs -> (p1 gs, p2 gs)) . parseGames

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

dzero :: Draw
dzero = Draw { red = 0, green = 0, blue = 0 }

dmerge :: Draw -> Draw -> Draw
dmerge d1 d2 = Draw { red = red d1 + red d2,
                      green = green d1 + green d2,
                      blue = blue d1 + blue d2 }

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
    draw = foldl dmerge dzero <$> count dzero `sepBy` char ','
    count d = between space space int >>= \i ->
      d { red = i } <$ string' "red" <|>
      d { green = i } <$ string' "green" <|>
      d { blue = i } <$ string' "blue"

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
