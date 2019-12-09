main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    let os = parseOrbit <$> ls
    let ocs = map (\(_,o) -> orbitCount o os) os
    print $ sum ocs 

parseOrbit :: String -> (String,String)
parseOrbit s = let (a,_:b) = break (==')') s in (a,b)

orbitCount :: String -> [(String,String)] -> Int
orbitCount "COM" _  = 0
orbitCount o xs = 1 + (orbitCount sun xs)
    where
    sun = (fst $ head $ filter ((==o) . snd) xs)
