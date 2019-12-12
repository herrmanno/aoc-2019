module Main(main) where

type Triple = (Int, Int, Int)

data Moon = Moon { pos :: Triple, vel :: Triple } deriving (Show)

type System = [Moon]

inputSystem =
    [ Moon { pos = (-7,-8,9), vel = (0,0,0) }
    , Moon { pos = (-12,-3,-4), vel = (0,0,0) }
    , Moon { pos = (6,-17,-9), vel = (0,0,0) }
    , Moon { pos = (4,-10,-6), vel = (0,0,0) }
    ]

testSystem =
    [ Moon { pos = (-1,0,2), vel = (0,0,0) }
    , Moon { pos = (2,-10,-7), vel = (0,0,0) }
    , Moon { pos = (4,-8,8), vel = (0,0,0) }
    , Moon { pos = (3,5,-1), vel = (0,0,0) }
    ]

step 0 s = s
step i s = let
    s' = map (updateMoon s) s
    in step (i - 1) s'

updateMoon [] m@Moon { pos = (x,y,z), vel = (vx,vy,vz) } = m { pos = (x+vx,y+vy,z+vz) }
updateMoon (m2:xs) m = let
    (x,y,z) = pos m
    (vx, vy, vz) = vel m
    (x2,y2,z2) = pos m2
    vx' = vx + cmp x x2
    vy' = vy + cmp y y2
    vz' = vz + cmp z z2
    -- pos' = (x + vx', y + vy', z + vz')
    m' = m { vel = (vx',vy',vz') }
    in updateMoon xs m'
    where
        cmp a a'
            | a < a' = 1
            | a > a' = -1
            | otherwise = 0

totalEnergy s = sum $ map moonEnergy s
    where
        moonEnergy Moon { pos = (x,y,z), vel = (a,b,c) } = sum' [x,y,z] * sum' [a,b,c]
        sum' = sum . (map abs)

main :: IO ()
main = do
    let s' = step 1000 inputSystem
    let e = totalEnergy s'
    print s'
    print e