import Data.List


bintodec :: [Bool] -> Int
bintodec xs = foldl (\acc x -> 2*acc+(fromEnum x)) 0 xs

main :: IO()
main = do
    content <- getContents
    let binList = lines content
        revList = transpose binList
        counts = map (\x -> length $ filter (=='1') x) revList
        greaters = map (\x -> x > (length binList) `div` 2) counts
        delta = bintodec greaters
        eps = bintodec $ map not greaters
        ans = delta*eps
    putStrLn $ show ans

