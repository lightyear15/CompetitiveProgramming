import Control.Monad (replicateM, liftM)
import Data.List (sort, group)

solve :: Int -> [Int] -> [Int]
solve k nums = unique $ filter (`elem` rawResult) nums
    where
        rawResult = map head $ filter (\lst -> length lst >= k) $ group $ sort nums
        unique :: [Int] -> [Int]
        unique [] = []
        unique (x:xs) = x : unique (filter (x /=) xs)

main = do
    testCases <- readLn :: IO Int
    replicateM testCases (do
            [_, k] <- fmap (map read . words) getLine :: IO [Int]
            solution <- fmap  (unwords . map show .solve k . map read . words) getLine
            if null solution
            then
                putStrLn "-1"
            else
                putStrLn solution
            )
