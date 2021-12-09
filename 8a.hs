
parseLine :: String -> ([String], [String])
parseLine s = (take 10 wds, drop 11 wds)
    where wds = words s

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        mp = map snd $ map parseLine lns
        flt = map (\x -> filter (\y -> (length y) `elem` [2,3,4,7]) x) mp
        ans = sum $ map length flt
    putStrLn $ show ans

