main = do
    txt <- readFile "input.txt"
    let (min, max) = break (== '-') txt
    let [min', max'] = map read [min, tail max]
    let n = length $ filter (==True) $ map matches [min'..max']
    print n

matches :: Int -> Bool
matches n = decreasing n && hastuple n
    where
        decreasing n =
            let s = show n
                xs = zip [0..] s
            in all (\(i,c) -> i == 0 || c >= s !! (i - 1)) xs
        hastuple n = 
            let s = show n
                xs = zip [1..] $ tail s
            in any (\(i,c) -> c == s !! (i - 1)) xs

