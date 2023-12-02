import Data.Text qualified as T

main :: IO ()
-- main = interact $ (++"\n")
main = interact $  unlines . concatMap (\l -> [l, (show . parse) l]) . take 1 . lines

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving Show

parse :: String -> Game
parse s = Game { gid = gid, draws = draws }
  where [gt, rt] = T.split (==':') (T.pack s)
        gid = read . T.unpack . last $ T.words gt
        draws = map parseDraw $ T.split (==';') rt

parseDraw :: T.Text -> Draw
parseDraw t = foldl update zero iters
    where zero = Draw { red = 0, green = 0, blue = 0 }
          iters = T.split (==',') t
          update d it = let [count, color] = T.words it in case T.unpack color of
            "red" -> d { red = (read . T.unpack) count }
            "green" -> d { green = (read . T.unpack) count }
            "blue" -> d { blue = (read . T.unpack) count }
