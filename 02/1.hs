main = do
    input <- readFile "input.txt"
    let splitted = split input
    let n = map (read :: String -> Int) splitted
    let n' = head n : 12 : 2 : drop 3 n
    let n'' = calc 0 n'
    -- putStrLn $ show $ n'' !! 0
    mapM putStrLn $ take 10 (map show n'')


calc :: Int -> [Int] -> [Int]
calc ip arr
    | arr !! ip == 99 = arr
    | arr !! ip == 1 = calc i4 (updateSet (+))
    | arr !! ip == 2 = calc i4 (updateSet (*))
    | otherwise = calc i4 arr
    where
        [i1, i2, i3, i4] = map (+ip) [1..4]
        [v2, v3, v4] = map (arr !!) [i2, i3, i4]
        -- updateSet op = set (arr !! i3) ((arr !! i1) + (arr !! i2)) arr
        updateSet op = take v4 arr ++ (op v2 v3) ++ drop (v4 + 1) arr

split "" = []
split xs = y : split rest
    where
        (y,ys) = break (==',') xs
        rest = if null ys then [] else tail ys

set :: Int -> a -> [a] -> [a]
set n v a = take n a ++ [v] ++ drop (n + 1) a