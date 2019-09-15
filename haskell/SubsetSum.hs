
import Control.Monad (replicateM)
import Data.List (findIndex, scanl1, sortBy)
import qualified Data.Vector as V


binSearch :: V.Vector Integer -> Integer -> Int -> Int -> Maybe Int
binSearch setOfA b start end
    | b <= setOfA V.! start = Just start
    | start == end = Nothing
    | b <= setOfA V.! mid = binSearch setOfA b start mid
    | otherwise = binSearch setOfA b (mid+1) end
    where
        mid = (start + end) `div` 2

solve :: V.Vector Integer -> Integer -> Int
solve setOfA b = maybe (-1) (1+) $ binSearch setOfA b 0 (V.length setOfA - 1)

main :: IO ()
main = do
    getLine
    setOfA <- V.fromList . scanl1 (+) . sortBy (flip compare) . map read . words <$> getLine :: IO (V.Vector Integer)
    testCount <- readLn :: IO Int
    solutions <- replicateM testCount $ solve setOfA <$> readLn :: IO[Int]
    putStrLn $ unlines $ map show solutions

