module Main(main) where

import Data.List (nub,sort)

type Triple = (Int, Int, Int)

data Moon = Moon { pos :: Triple, vel :: Triple } deriving (Show, Eq)
sx Moon { pos = (a,_,_), vel = (b,_,_) } = (a,b)
sy Moon { pos = (_,a,_), vel = (_,b,_) } = (a,b)
sz Moon { pos = (_,_,a), vel = (_,_,b) } = (a,b)

ssx (m1:m2:m3:m4:[]) = (sx m1, sx m2, sx m3, sx m4)
ssy (m1:m2:m3:m4:[]) = (sy m1, sy m2, sy m3, sy m4)
ssz (m1:m2:m3:m4:[]) = (sz m1, sz m2, sz m3, sz m4)


type System = [Moon]

inputSystem =
    [ Moon { pos = (-7,-8,9), vel = (0,0,0) }
    , Moon { pos = (-12,-3,-4), vel = (0,0,0) }
    , Moon { pos = (6,-17,-9), vel = (0,0,0) }
    , Moon { pos = (4,-10,-6), vel = (0,0,0) }
    ]

testSystem1 =
    [ Moon { pos = (-1,0,2), vel = (0,0,0) }
    , Moon { pos = (2,-10,-7), vel = (0,0,0) }
    , Moon { pos = (4,-8,8), vel = (0,0,0) }
    , Moon { pos = (3,5,-1), vel = (0,0,0) }
    ]

testSystem2 =
    [ Moon { pos = (-8,-10,0), vel = (0,0,0) }
    , Moon { pos = (5,5,10), vel = (0,0,0) }
    , Moon { pos = (2,-7,3), vel = (0,0,0) }
    , Moon { pos = (9,-8,-3), vel = (0,0,0) }
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
    m' = m { vel = (vx',vy',vz') }
    in updateMoon xs m'
    where
        cmp a a'
            | a < a' = 1
            | a > a' = -1
            | otherwise = 0

repeatsAfter s = let
    fs =
        [ a . b
        | a <- [ssx,ssy,ssz]
        , b <- [id]
        ]
    -- in map (\f -> repeatsAfterBy [] f s) fs -- <- way slower
    in repeatsAfter' 1 [] [] fs s

repeatsAfter':: (Eq a) => Int -> [a] -> [Maybe Int] -> [(System -> a)] -> System -> [Int]
repeatsAfter' _ [] _ fs s = repeatsAfter' 1 (map (\f -> f s) fs) (replicate (length fs) Nothing) fs s
repeatsAfter' i val res fs s = let
    s' = step 1 s
    res' = map (getRes s') $ zip3 fs res val
    finished = Nothing /= sequence res'
    in if finished then unwrap (sequence res') else repeatsAfter' (i + 1) val res' fs s'
    where
        getRes _ (_, res@(Just x), _) = res
        getRes s' (f, _, v) = if v  == f s' then Just i else Nothing
        unwrap (Just x) = x

-- works but is way slower
repeatsAfterBy [] f s = repeatsAfterBy [f s] f s
repeatsAfterBy acc f s = let
    s' = step 1 s
    sn = f s'
    l = length acc
    matches = sn `elem` acc -- sn == last acc
    in if matches then l else repeatsAfterBy (sn:acc) f s'



main :: IO ()
main = do
    let fx =  repeatsAfter inputSystem
    print $ fx
    print $ foldl1 lcm fx
