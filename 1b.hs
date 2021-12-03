
stringLToInt :: [String] -> [Int]
stringLToInt xs = map read xs

sumThreeTake :: [Int] -> Int
sumThreeTake xs = sum $ take 3 xs

slidingWindow :: [Int] -> [Int]
slidingWindow [] = []
slidingWindow all@(x:xs)
    | length all < 3 = []
    | otherwise = (sumThreeTake all):(slidingWindow xs)

numIncrease :: [Int] -> Int
numIncrease xs = length $ filter (<0) $ zipWith (-) xs (tail xs)

main :: IO()
main = do
    content <- getContents
    let intList = stringLToInt $ lines content
        slideList = slidingWindow intList
        incNum = numIncrease slideList
    putStrLn $ show incNum
