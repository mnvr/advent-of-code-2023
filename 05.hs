import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parseAlmanac

data Almanac = Input { seeds :: [Int], maps :: [Map] }
data Map = Map { sourceRange :: Int, destinationRange :: Int, length :: Int } deriving Show

-- parseAlmanac :: String -> Input
parseAlmanac s = case parse almanac "" s of
    (Left err)-> error (show err)
    (Right v) -> v
  where almanac = string "seeds"

p1 = id
