import Text.Parsec (ParseError, digit, many1, parse, sepBy, spaces, string, string', eof, space, char, endBy, sepEndBy, optional, (<|>))
import Text.Parsec.String (Parser)

-- Parse a single integer
integer :: Parser Integer
integer = read <$> many1 digit

-- Parse a list of integers separated by spaces
integers :: Parser [Integer]
integers = integer `sepBy` spaces

-- Parse an integer from a string
parseInteger :: String -> Either ParseError [Integer]
parseInteger = parse integers ""

game :: Parser String
game = string' "Game"

red :: Parser String
red = string' "red"

green :: Parser String
green = string' "green"

blue :: Parser String
blue = string' "blue"

colon :: Parser Char
colon = char ':'

semicolon :: Parser Char
semicolon = char ';'

comma :: Parser Char
comma = char ','

-- myParser :: String -> Either ParseError [Integer]
myParser = parse parser ""

parser = do
  game
  space
  i <- integer
  colon
  ds <- draws
  eof
  pure $ (i, ds)

draws = draw `sepEndBy` semicolon

draw = count `sepBy` comma

data C = Red | Green | Blue deriving Show

count = do
  space
  b <- integer
  space
  l <- red <|> green <|> blue
  t <- case l of
    "red" -> pure Red
    "green" -> pure Green
    "blue" -> pure Blue
  return (b, t)

main :: IO ()
main = do
  let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
