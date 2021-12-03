
stringLToInt :: [String] -> [Int]
stringLToInt = map read

numIncrease :: [Int] -> Int
numIncrease xs = length $ filter (<0) $ zipWith (-) xs (tail xs)

main :: IO()
main = do
    content <- getContents
    let intList = stringLToInt $ lines content
        incNum = numIncrease intList
    putStrLn $ show incNum

