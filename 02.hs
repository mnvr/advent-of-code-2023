import Text.Parsec
import Text.Parsec.String (Parser)

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
parseGames s = case parse games "" s of
    Left err -> error (show err)
    Right g -> g
  where
    games = game `sepBy` newline
    game = do
      i <- string "Game " *> int <* char ':'
      ds <- draw `sepBy` char ';'
      return Game { gid = i, draws = ds }
    int = read <$> many1 digit
    draw = chainl count (char ',' >> pure (<>)) mempty
    count = between space space int >>= \i ->
      Draw i 0 0 <$ string "red" <|>
      Draw 0 i 0 <$ string "green" <|>
      Draw 0 0 i <$ string "blue"
