import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between, newline)
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Control.Exception (throw)

data C = Red | Green | Blue deriving Show

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

dzero :: Draw
dzero = Draw 0 0 0

dmerge :: Draw -> Draw -> Draw
dmerge d1 d2 = Draw { red = red d1 + red d2,
                      green = green d1 + green d2,
                      blue = blue d1 + blue d2 }

int :: Parser Int
int = read <$> many1 digit

-- myParser :: String -> Either ParseError [Integer]
myParser = parse parser ""

parser = do
    gs <- game `sepBy` newline
    eof
    return gs
  where
    game = do
      i <- between (string' "Game" >> space) colon int
      ds <- draws
      return Game { gid = i, draws = ds }
    colon = char ':'
    semicolon = char ';'
    comma = char ','
    draws = draw `sepBy` semicolon
    draw = foldl dmerge dzero <$> count dzero `sepBy` comma
    count d = between space space int >>= \i ->
      d { red = i } <$ string' "red" <|>
      d { green = i } <$ string' "green" <|>
      d { blue = i } <$ string' "blue"

main :: IO ()
main = interact $ \input ->
  case myParser input of
    Left err -> throw (userError (show err))
    Right n -> "Parsed: " ++ show n ++ "\n"
