import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse, between)
import Text.Parsec.String (Parser)

data C = Red | Green | Blue deriving Show

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

dzero :: Draw
dzero = Draw 0 0 0

int :: Parser Int
int = read <$> many1 digit

-- myParser :: String -> Either ParseError [Integer]
myParser = parse parser ""

parser = do
    game
    i <- between space colon int
    ds <- draws
    eof
    return (i,ds)
    -- return Game { gid = i, draws = ds }
  where
    game = string' "Game"
    colon = char ':'
    semicolon = char ';'
    comma = char ','
    draws = draw `sepEndBy` semicolon
    draw = count `sepBy` comma
    count = between space space int >>= \i ->
      dzero { red = i } <$ string' "red" <|>
      dzero { green = i } <$ string' "green" <|>
      dzero { blue = i } <$ string' "blue"

main :: IO ()
main = do
  let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
