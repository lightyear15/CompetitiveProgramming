solve :: String -> String -> String
solve _ [] = []
solve [] _ = []
solve (x:xs) (y:ys) 
    | x /= y = []
    | x == y = x : solve xs ys

main = do
    x <- getLine
    y <- getLine
    let prefix = solve x y
    let prefixLen = length prefix
    let xPrefixed = drop prefixLen x
    let yPrefixed = drop prefixLen y
    putStrLn (show (prefixLen) ++ " " ++ prefix)
    putStrLn ( show (length xPrefixed) ++ " " ++ xPrefixed)
    putStrLn ( show (length yPrefixed) ++ " " ++ yPrefixed)
