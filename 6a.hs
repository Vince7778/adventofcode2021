import Data.List
import Data.Function (on)

parseLine :: String -> [Int]
parseLine s =
    let groupComma = groupBy ((==) `on` (==',')) s
        filtered = filter (/=",") groupComma
    in map read filtered

groupList :: [Int] -> [(Int, Int)]
groupList xs = map (\all@(x:xs) -> (x, length all-1)) grouped
    where grouped = group $ sort $ xs ++ [0..8]

calcDay :: [(Int, Int)] -> [(Int, Int)]
calcDay xs =
    let subt = map (\(a,b) -> (a-1,b)) xs
        elemn1 = snd $ head $ filter (\(a,b) -> a == (-1)) subt
        added = map (\(a,b) -> if (a /= 6) then (a,b) else (a,b+elemn1)) subt
    in tail (added ++ [(8, elemn1)])

iterDay :: [(Int, Int)] -> [(Int, Int)]
iterDay xs = (iterate calcDay xs) !! 80

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        parsed = parseLine $ head lns
        ans = sum $ map snd $ iterDay $ groupList parsed
    putStrLn $ show ans
