import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Text.Printf

-- FindRoot find the closest integer less than equal to the nth root of x
findRoot :: Integer -> Integer -> Integer -> Integer -> Integer
findRoot x n lo up
    | lo == up= lo
    | up - lo == 1 = lo
    | (up -1)^n > x = findRoot x n lo (up-1)
    | (lo +1)^n <= x = findRoot x n (lo+1) up
    | otherwise = error (printf " x:%d n:%d l:%d u:%d " x n lo up)

-- innerSolve given x and n and the partial result, build a list of possible candidate for the next step
innerSolve :: Integer -> Integer -> ([Integer],Integer) -> [([Integer],Integer)]
innerSolve 0 _ _ = []
innerSolve x n (ps,sumOfPs)
    | remain < 0 = []
    | remain == 0 = [(ps,sumOfPs)]
    | otherwise = concatMap (\e -> innerSolve x n (e:ps, e^n + sumOfPs)) [i | i <-[biggest+1..root], i^n + sumOfPs <= x]
    where
        biggest = case ps of [] -> 0
                             _ -> maximum ps
        remain = x - sumOfPs
        root = findRoot remain n 0 remain


solve :: [Integer] -> Int
solve [x,n] = length $ Set.fromList $ map (sort . fst) $ innerSolve x n ([],0)
solve a = error "input not expected"


main = interact $ show . solve . map read . words
