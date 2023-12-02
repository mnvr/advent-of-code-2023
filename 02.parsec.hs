import Text.Parsec (ParseError, digit, many1, parse, sepBy, spaces, string, string', eof, space, char, endBy, sepEndBy, optional)
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
  d <- draw
  eof
  pure $ (i, d)

draw = do
  cs <- count `sepBy` comma
  optional semicolon
  return cs

count = do
  space
  b <- integer
  space
  blue
  return b

main :: IO ()
main = do
  -- let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  let input = "Game 18: 3 blue, 4 blue, 5 blue;"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
