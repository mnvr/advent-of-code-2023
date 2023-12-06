-- A simpler, barebones version of the solution with a foldr.

import Data.Bifunctor

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseCards

-- parseCards :: String -> [([Int], [Int])]
parseCards s = map parseCard $ lines s
  where parseCard s' = second tail . span (/= '|') $ dropWhile (/= ':') s'

p1 = id
p2 = p1
