import Text.Parsec

main :: IO ()
main = interact $ (++"\n") . show . ((,) <$> p1 <*> p2) . parseGames

data Game = Game { gid :: Int, draw :: Draw } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

instance Semigroup Draw where
  (Draw a b c) <> (Draw x y z) = Draw (max a x) (max b y) (max c z)

instance Monoid Draw where
  mempty = Draw 0 0 0

possible :: Game -> Bool
possible = valid . draw
  where valid (Draw r g b) = r <= 12 && g <= 13 && b <= 14

p1 :: [Game] -> Int
p1 = sum . map gid . filter possible

power :: Draw -> Int
power (Draw r g b) = r * g * b

p2 :: [Game] -> Int
p2 = sum . map (power . draw)

parseGames :: String -> [Game]
parseGames s = case parse games "" s of
    Left err -> error (show err)
    Right g -> g
  where
    games = game `sepBy` newline
    game = Game <$> (string "Game " *> int <* char ':') <*> draws
    int = read <$> many1 digit
    draws = chainl draw (char ';' >> pure (<>)) mempty
    draw = chainl count (char ',' >> pure (<>)) mempty
    count = between space space int >>= \i ->
      Draw i 0 0 <$ string "red" <|>
      Draw 0 i 0 <$ string "green" <|>
      Draw 0 0 i <$ string "blue"
