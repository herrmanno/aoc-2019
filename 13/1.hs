import IntCode (initialState,run,output)

main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]"
    let s = run $ initialState code []
    let o = zip [1..] (tail $ output s)
    let tileTypes = filter ((==0) . (`mod`3) . fst) o
    print $ map snd tileTypes
    print $ length $ filter ((==2) . snd) $ tileTypes
