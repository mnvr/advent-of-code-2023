{-# OPTIONS_GHC -Wno-all #-}
main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse = (head) . lines

p1 = id
