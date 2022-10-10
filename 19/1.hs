import IntCode

main = do
    input <- readFile "input.txt"
    let code = read $ "[" ++ input ++ "]"
    let result =
            [ head . output $ run (initialState code [x, y])
            | x <- [0..49]
            , y <- [0..49]
            ]
    print $ length . filter (==1) $ result

