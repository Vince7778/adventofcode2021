import Data.List

bintodec :: [Bool] -> Int
bintodec xs = foldl (\acc x -> 2*acc+(fromEnum x)) 0 xs

stringtobin :: String -> [Bool]
stringtobin xs = map (=='1') xs

calcCommon :: [String] -> [Bool]
calcCommon xs =
    let revList = transpose xs
        counts = map (\x -> length $ filter (=='1') x) revList
        greaters = map (\x -> x >= ((length xs + 1) `div` 2)) counts
    in greaters

recurse :: [String] -> Int -> Char -> String
recurse strs ind inv
    | length strs == 1 = head strs
    | (ind >= length (head strs)) = head strs
    | otherwise =
        let com = calcCommon strs
            cbool = (com !! ind)
            filtL = \c -> ((c !! ind) == inv) == cbool
            filtStrs = filter filtL strs
        in recurse filtStrs (ind+1) inv

main :: IO()
main = do
    content <- getContents
    let binList = lines content
        oxy = recurse binList 0 '1'
        co2 = recurse binList 0 '0'
        conv = bintodec . stringtobin
        ans = (conv oxy) * (conv co2)
    putStrLn $ show ans
