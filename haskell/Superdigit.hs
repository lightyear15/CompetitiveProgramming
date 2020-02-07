
import Data.Char (digitToInt)

solve :: String -> Integer
solve x_str 
    |  x < 10 = x
    | otherwise = solve $ show $ sum $ map digitToInt x_str
    where
        x = read x_str :: Integer

main :: IO ()
main = do
    [ns, ks] <- words <$> getLine
    let k = read ks :: Integer
    print $ solve $ show (k * solve ns)

