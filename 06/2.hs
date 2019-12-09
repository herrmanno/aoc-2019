import Data.List (elemIndex)

type Orbit = (String,String)
type Path = [String]

main :: IO ()
main = do
    os <- (fmap parseOrbit) <$> lines <$> readFile "input.txt"
    let paths = (`orbitPath` os) <$> ["YOU", "SAN"]
    let common = let (p1:p2:[]) = paths in commonStar p1 p2
    let ds = map (pathDist common) paths
    print $ sum ds

parseOrbit :: String -> Orbit
parseOrbit s = let (a,_:b) = break (==')') s in (a,b)

pathDist :: String -> Path -> Int
pathDist star p = let Just d = (star `elemIndex` p) in d

commonStar :: Path -> Path -> String
commonStar [] _ = error "No common star"
commonStar (x:xs) ys = if x `elem` ys then x else commonStar xs ys

orbitPath :: String -> [Orbit] -> Path
orbitPath "COM" _  = []
orbitPath o xs = sun:(orbitPath sun xs)
    where
    sun = (fst $ head $ filter ((==o) . snd) xs)
