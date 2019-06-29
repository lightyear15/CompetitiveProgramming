import Data.List

solve :: String -> String
solve st = concatMap toString $ map (\l -> (head l, length l)) $ Data.List.groupBy (\a b -> a == b) st
    where 
        toString (c, 1) = c : []
        toString (c, l) = c : show l
main = interact solve
