drawLine :: [Int] -> [Int]
drawLine [x, y] = [x+y]
drawLine (a:b:rest) = (a+b) : drawLine (b:rest)

fakeLine :: [Int] -> [Int]
fakeLine li = 0 : li ++ [0]

drawTriangle :: Int -> [[Int]]
drawTriangle n = take n $ iterate ( drawLine .fakeLine ) [1]

main = interact $ unlines . map (unwords . map show) . drawTriangle . read

