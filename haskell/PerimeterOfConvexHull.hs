import Control.Monad
import Data.List

type Point = (Float, Float)

lowestComparator :: Point -> Point -> Ordering
lowestComparator (ax,ay) (bx,by) 
    | ay < by || (ay == by && ax < bx) = LT
    | ay == by && ax == bx = EQ
    | otherwise = GT

polarComparator :: Point -> Point -> Point -> Ordering
polarComparator (ox,oy) (ax,ay) (bx,by) = compare (atan2(ay-oy)( ax - ox)) (atan2 (by - oy) (bx - ox))

cross :: Point -> Point -> Point -> Float
cross (ox,oy) (ax,ay) (bx,by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

convexHullIntro :: [Point] -> [Point]
convexHullIntro points = sortBy (polarComparator $ head lowestSorted) lowestSorted
    where lowestSorted = sortBy lowestComparator points

convexHull :: [Point] -> [Point] -> [Point]
convexHull stack [] = stack
convexHull [] (p:rest) = convexHull [p] rest
convexHull [top] (p:rest) = convexHull [p, top] rest
convexHull (top:nextToTop:restStack) (p:rest) 
    | cross nextToTop top p <= 0 = convexHull (nextToTop:restStack) (p:rest)
    | otherwise = convexHull (p:top:nextToTop:restStack) rest 

distance :: Point -> Point -> Float
distance (ax, ay) (bx, by) =sqrt ((ax - bx) * (ax - bx) + (ay - by) * (ay - by))

perimeterHull :: [Point] -> Float
perimeterHull points = perimeterHull' (points ++ [head points])

perimeterHull' :: [Point] -> Float
perimeterHull' [p1,p2] = distance p1 p2
perimeterHull' (p1:p2:rest) = distance p1 p2 + perimeterHull' (p2:rest)

readPoint :: IO Point
readPoint = do
    ln <- getLine
    let [a,b] = map read $ words ln
    return (a,b)


main = do 
    pointCount <- readLn :: IO Int
    points <- replicateM pointCount readPoint
    print $ perimeterHull $ convexHull [] $ convexHullIntro points
