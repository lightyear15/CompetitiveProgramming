import Control.Monad (replicateM)

thePower :: Int
thePower = 10^8 + 7

solve :: Int-> Int
solve = (map fib [0..] !!)
    where
        fib 0 = 0
        fib 1 = 1
        fib n = (solve (n-1) `mod` thePower + solve (n-2) `mod` thePower ) `mod` thePower


main = do
    testCases <- readLn :: IO Int
    replicateM testCases (print . solve =<< readLn)
