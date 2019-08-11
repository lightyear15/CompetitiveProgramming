import Control.Monad

-- Red, Green, Yellow, Blue
type BallCount = (Integer, Integer, Integer, Integer)

countBall :: BallCount -> Char -> BallCount
countBall (r,g,y,b) color 
    | color == 'R' = (r+1,g,y,b)
    | color == 'G' = (r,g+1,y,b)
    | color == 'Y' = (r,g,y+1,b)
    | color == 'B' = (r,g,y,b+1)

-- (Red - Green, Blue - Yellow) 
type PrefixCount  = (Int, Int)
countPrefix :: [PrefixCount] -> Char -> [PrefixCount]
countPrefix prefixHistory color = case prefixHistory of
    [] -> [(r_g1,b_y1)]
    ((r_g,b_y):rest) -> (r_g + r_g1, b_y + b_y1) : prefixHistory
    where (r_g1, b_y1) = (fromEnum (color == 'R') - fromEnum (color == 'G'), fromEnum(color == 'B') - fromEnum(color == 'Y'))

checkBallPrefix :: String -> Bool
checkBallPrefix ballsSequence = all checkPrefix prefixStory
    where
        prefixStory =  foldl countPrefix [] ballsSequence
        checkPrefix :: PrefixCount -> Bool
        checkPrefix (r_g, b_y) = abs r_g <= 1 && abs b_y <= 1

solve :: String -> Bool
solve colorSequence = rc == gc && yc == bc && checkBallPrefix colorSequence
    where 
        (rc,gc,yc,bc) = foldl countBall (0,0,0,0) colorSequence

main = do
    tests <- readLn :: IO Int
    replicateM tests ( print . solve =<< getLine )
