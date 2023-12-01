    import Data.Char (isDigit, digitToInt)
    import Data.List (isPrefixOf, findIndex)

    main :: IO ()
    main = interact $ (++ "\n") . show . sum . fmap parse . lines

    parse :: String -> Int
    parse s = first s spelled * 10 + first (reverse s) (map reverse spelled)

    first :: String -> [String] -> Int
    first s@(c:s') ms = if isDigit c then digitToInt c else
        case findIndex (`isPrefixOf` s) ms of
            Nothing -> first s' ms
            (Just i) -> i + 1

    spelled :: [String]
    spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
