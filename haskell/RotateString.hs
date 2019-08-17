import Control.Monad (replicateM)


solve :: String -> [String]
solve st = take (length st) $ map (rotate st) [1..]
    where 
        rotate :: [a] -> Int -> [a]
        rotate l i = drop i l ++ take i l 

main = do
    testCases <- readLn :: IO Int
    replicateM testCases (putStrLn . unwords . solve =<< getLine)
