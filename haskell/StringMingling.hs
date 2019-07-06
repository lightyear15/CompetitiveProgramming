solve :: String -> String -> String
solve [] [q] = [q]
solve [p] [] = [p]
solve p@(p1:ps) q@(q1:qs) = p1 : solve q ps

main = do
    p <- getLine
    q <- getLine
    putStrLn $ solve p q
