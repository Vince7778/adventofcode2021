import Data.List (groupBy, transpose)
import Data.Function (on)

orderSwap :: (Int, Int) -> (Int, Int)
orderSwap (a, b) = if a < b then (a, b) else (b, a)

doubleSwap :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
doubleSwap all@((a, b), (c, d)) = if b < d then all else ((c, d), (a, b))

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s =
    let wds = words s
        groupComma = groupBy ((==) `on` (==','))
        tuplify t = (read (head $ groupComma t), read (last $ groupComma t))
    in (tuplify (head wds), tuplify (last wds))

addToSublist :: [Int] -> (Int, Int) -> [Int]
addToSublist xs (a, b) =
    let (front, back) = splitAt a xs
        modList = map succ $ take (b-a+1) back
    in front ++ modList ++ drop (b-a+1) back 

incrementInd :: [Int] -> Int -> [Int]
incrementInd [] _ = []
incrementInd (x:xs) 0 = (x+1):xs
incrementInd (x:xs) ind = x:incrementInd xs (ind-1)

addToRow :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
addToRow [] _ _ = []
addToRow (r:xs) t 0 = addToSublist r t : xs
addToRow (r:xs) t row = r : addToRow xs t (row-1)

addToCol :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
addToCol xs t col = transpose $ addToRow (transpose xs) t col

addToDiag :: [[Int]] -> ((Int, Int), (Int, Int)) -> [[Int]]
addToDiag [] _ = []
addToDiag all@(x:xs) ((a, b), (c, d))
    | b < 0 && d < 0 = all
    | b > 0 && d > 0 = x:addToDiag xs ((a, b-1), (c, d-1))
    | otherwise = incrementInd x a : addToDiag xs ((aval, b-1), (c, d-1))
        where aval = if a > c then a-1 else a+1

placeLine :: [[Int]] -> ((Int, Int), (Int, Int)) -> [[Int]]
placeLine xs ((a, b), (c, d)) =
    if a == c then
        addToCol xs (orderSwap (b, d)) a
    else if b == d then
        addToRow xs (orderSwap (a, c)) b
    else
        addToDiag xs $ doubleSwap ((a, b), (c, d))

placeLines :: [((Int, Int), (Int, Int))] -> [[Int]]
placeLines lns = foldl (\acc x -> placeLine acc x) grid lns
    where grid = map (\x -> take 1000 $ repeat 0) $ take 1000 $ repeat 0

countTwos :: [[Int]] -> Int
countTwos xs = foldl (\acc x -> acc + length (filter (>=2) x)) 0 xs

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        parsed = map parseLine lns
        filled = placeLines parsed
        ans = countTwos filled
    putStrLn $ show ans
