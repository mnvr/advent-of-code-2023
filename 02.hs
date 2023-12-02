main :: IO ()
main = interact $ (++"\n") . show . map (gid . parse) . lines

data Game = Game { gid :: Int, draws :: [Int]}

parse :: String -> Game
parse s = Game { gid = 0, draws = []}
