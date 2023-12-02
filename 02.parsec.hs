import Text.Parsec (ParseError, digit, many1, parse, sepBy, spaces, string, string', eof, space, char, endBy, sepEndBy, optional, (<|>))
import Text.Parsec.String (Parser)
import Data.Functor (($>))

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

red :: Parser C
red =  Red <$ string' "red"

green :: Parser C
green = Green <$ string' "green"

blue :: Parser C
blue = Blue <$ string' "blue"

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
  t <- red <|> green <|> blue
  return (b, t)

main :: IO ()
main = do
  let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
