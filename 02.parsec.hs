import Text.Parsec (string', char, digit, many1, eof, sepEndBy, sepBy, space, (<|>), parse)
import Text.Parsec.String (Parser)

data C = Red | Green | Blue deriving Show

game :: Parser String
game = string' "Game"

integer :: Parser Integer
integer = read <$> many1 digit

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
  pure (i, ds)

draws = draw `sepEndBy` semicolon

draw = count `sepBy` comma

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
