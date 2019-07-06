import Control.Monad

solve :: String -> String
solve [] = []
solve (a:b:rest) = b : a : solve rest

main = do
    cases <- readLn :: IO Int
    answers <- replicateM cases $ fmap solve getLine
    putStrLn $ unlines answers
