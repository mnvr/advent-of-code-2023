import Text.Parsec (ParseError, digit, many1, parse, sepBy, spaces, string, string', eof)
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

myParser :: String -> Either ParseError ()
myParser = parse parser ""
  where parser = game >> eof

main :: IO ()
main = do
  -- let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  let input = "Game"
  case myParser input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
