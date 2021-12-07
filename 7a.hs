import Data.List
import Data.Function (on)

parseLine :: String -> [Int]
parseLine s =
    let groupComma = groupBy ((==) `on` (==',')) s
        filtered = filter (/=",") groupComma
    in map read filtered

getDiff :: [Int] -> Int -> Int
getDiff xs v = sum $ map (\x -> abs (x-v)) xs

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        parsed = parseLine $ head lns
        ans = minimum $ map (getDiff parsed) [1..1500]
    putStrLn $ show ans
