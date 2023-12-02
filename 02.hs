import Data.Text qualified as T

main :: IO ()
-- main = interact $ (++"\n")
main = interact $  unlines . concatMap (\l -> [l, (show . parse) l]) . lines
-- main = interact $  unlines . map (id) . lines

data Game = Game { gid :: Int, draws :: [Draw] } deriving Show
type Draw = (Int,Int,Int)

parse :: String -> Game
parse s = Game { gid = gid, draws = draws }
  where [gt, rt] = T.split (==':') (T.pack s)
        gid = read . T.unpack . last $ T.words gt
        draws = map draw $ T.split (==';') rt
        draw t = (0,0,0)
