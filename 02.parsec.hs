import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between)
import Text.Parsec.String (Parser)

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
    i <- between (game >> space) colon int
    ds <- draws
    eof
    return Game { gid = i, draws = ds }
  where
    game = string' "Game"
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
main = do
  let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
