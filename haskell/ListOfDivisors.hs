import Control.Monad (replicateM)

solve :: Int -> Int -> Int
solve a b = length [n | n <-[1.. gcdab], a `mod` n == 0, b `mod` n == 0]
    where
        minab = gcd a b

main :: IO ()
main = do
    testCount <- readLn :: IO Int
    solutions <- replicateM testCount $ (uncurry solve . (\[a,b] -> (a,b)) . map read . words) <$> getLine
    putStrLn $ unlines $ map show solutions

