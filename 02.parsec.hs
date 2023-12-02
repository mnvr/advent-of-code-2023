import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between, newline)
import Text.Parsec.String (Parser)
import Control.Exception (throw)

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

dzero :: Draw
dzero = Draw 0 0 0

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

main :: IO ()
main = interact (show . parseGames)
