import Control.Monad (forM_)
main = do
    (input :: [Int]) <- (fmap (read . (:[]))) <$> readFile "input.txt"

    let outputs1 = iterate calculate input
    let output1 = outputs1 !! 100
    putStrLn $ "Part 1: " ++ (concatMap show . take 8 $ output1)

    let offset = read . concatMap show . take 7 $ input
    let input2 = drop offset . concat . (replicate 10000) $ input
    let outputs2 = fmap reverse $ iterate calculate3 (reverse input2)
    let output2 = outputs2 !! 100
    putStrLn $ "Part 2: " ++ (concatMap show . take 8 $ output2)

calculate :: [Int] -> [Int]
calculate inputs = go inputs [0..(length inputs - 1)]
    where
        go :: [Int] -> [Int] -> [Int]
        go inputs [] = []
        go inputs (idx:indices) =
            let patterns = drop 1 . cycle . concatMap (replicate (idx + 1)) $ [0, 1, 0, -1]
                digit = lastDigit . sum $ zipWith (*) inputs patterns
            in digit : go inputs indices
        lastDigit i
            | i >= 0 = i `mod` 10
            | otherwise = (-1 * i) `mod` 10

-- | At the second half of the input, the calcuation is pretty simple, because the multiplication
--   schema looks like [0,0,0,0, .. 1,1,1,1,1,1,1,1..]
--                                  ^ everything until here (that is the index of the number we
--                                    want to calculate) is zero.
--   So to calculate numbers of the input's second half one only need to sum all numbers from the
--   index in question to the end.
--
--   This can be further improved when starting at the back. The last number always stays the same
--   (following the schema described above). The number before that number (at index n) will be the
--   sum of old_{n} + new_{n+1} % 10. This pattern continues to work for all numbers in the input's
--   second half.
calculate3 :: [Int] -> [Int]
calculate3 inputs = go inputs 0
    where
        go :: [Int] -> Int -> [Int]
        go [] _ = []
        go (x:xs) s =
            let x' = (x + s) `rem` 10
            in x' : go xs x'