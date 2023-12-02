import Text.Parsec (ParseError, digit, many1, parse)
import Text.Parsec.String (Parser)

-- Parse a single integer
integer :: Parser Integer
integer = read <$> many1 digit

-- Parse an integer from a string
parseInteger :: String -> Either ParseError Integer
parseInteger = parse integer ""

main :: IO ()
main = do
  let input = "e123"
  case parseInteger input of
    Left err -> fail (show err)
    Right n -> putStrLn $ "Parsed: " ++ show n
