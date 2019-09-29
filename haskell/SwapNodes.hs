
import Control.Monad (replicateM, foldM)
import Debug.Trace


data Tree a = EmptyNode | Node  a (Tree a) (Tree a) deriving (Show, Eq)

type TreeNotation = (Int, Int)

buildTree :: [TreeNotation] -> Int -> Tree Int
buildTree treeTextFormat idx = case currentNode of
                                    (-1, -1) -> Node idx EmptyNode EmptyNode
                                    (-1, n) ->  Node idx EmptyNode (buildTree treeTextFormat n)
                                    (n, -1) -> Node idx (buildTree treeTextFormat n) EmptyNode
                                    (n , m) -> Node idx (buildTree treeTextFormat n) (buildTree treeTextFormat m)
    where 
        currentNode = treeTextFormat !! (idx -1)

treeDepth :: Tree a -> Int
treeDepth EmptyNode = 0
treeDepth (Node _ lTree rTree) = 1 + max (treeDepth lTree) ( treeDepth rTree)

swapNodes :: Tree a -> Int -> Tree a
swapNodes EmptyNode level = EmptyNode
swapNodes (Node a ltree rtree) level = if level == 0 
                                        then Node a rtree ltree
                                        else Node a (swapNodes ltree (level -1)) (swapNodes rtree (level-1))

treeToString :: Tree Int -> String
treeToString EmptyNode = ""
treeToString (Node val ltree rtree) = treeToString ltree ++  show val ++ " " ++ treeToString rtree


main = do
    n <- readLn :: IO Int
    treeTextFormat <- replicateM n ((\[a,b] -> (a,b)) . map read . words <$> getLine) :: IO[TreeNotation]
    let initialTree = buildTree treeTextFormat 1
    let treeDpt = treeDepth initialTree
    t <- readLn :: IO Int
    swaps <- replicateM t readLn :: IO [Int]
    foldM (swapAndPrint treeDpt) initialTree swaps
    where 
        swapAndPrint :: Int -> Tree Int -> Int -> IO (Tree Int)
        swapAndPrint treeDepth tree k = do 
            let swappedTree = foldl swapNodes tree [k*i -1 | i <- [1..treeDepth `div` k]]
            putStrLn $ treeToString swappedTree
            return swappedTree
