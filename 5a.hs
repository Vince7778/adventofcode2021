import Data.List (groupBy, transpose)
import Data.Function (on)

orderSwap :: (Int, Int) -> (Int, Int)
orderSwap (a, b) = if a < b then (a, b) else (b, a)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s =
    let wds = words s
        groupComma = groupBy ((==) `on` (==','))
        tuplify t = (read (head $ groupComma t), read (last $ groupComma t))
    in (tuplify (head wds), tuplify (last wds))

isStraight :: ((Int, Int), (Int, Int)) -> Bool
isStraight ((a, b), (c, d)) = a == c || b == d

addToSublist :: [Int] -> (Int, Int) -> [Int]
addToSublist xs (a, b) =
    let (front, back) = splitAt a xs
        modList = map succ $ take (b-a+1) back
    in front ++ modList ++ drop (b-a+1) back 

addToRow :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
addToRow xs t row =
    let (front, r:back) = splitAt row xs
        changedRow = addToSublist r t
    in front ++ changedRow : back

addToCol :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
addToCol xs t col = transpose $ addToRow (transpose xs) t col

placeLine :: [[Int]] -> ((Int, Int), (Int, Int)) -> [[Int]]
placeLine xs ((a, b), (c, d)) =
    if a == c then
        addToCol xs (orderSwap (b, d)) a
    else
        addToRow xs (orderSwap (a, c)) b

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
        filtered = filter isStraight parsed
        filled = placeLines filtered
        ans = countTwos filled
    putStrLn $ show ans
