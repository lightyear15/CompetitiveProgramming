solve :: String -> String
solve [] = []
solve (c:rest)
    | elem c rest = solve rest
    | otherwise = c : solve rest

main = interact $ reverse . solve . reverse
