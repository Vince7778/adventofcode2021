import Data.List

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

readBoard :: [String] -> [Int]
readBoard strs =
    let ss = take 5 strs
        mps = concat $ map words ss
    in map (read :: String -> Int) mps

rowWins :: [Int] -> [Int] -> Bool
rowWins board drawn
    | length board < 5 = False
    | otherwise =
        let curRow = take 5 board
        in (all (\x -> elem x drawn) curRow) || (rowWins (drop 5 board) drawn)

everyNth :: Int -> [Int] -> [Int]
everyNth _ [] = []
everyNth n a@(x:xs) = x:(everyNth n $ drop n a)

transpose' :: [Int] -> [Int]
transpose' xs = concat $ map (\x -> everyNth 5 $ drop x xs) [0..4]

boardWins :: [Int] -> [Int] -> Bool
boardWins drawn board = 
    let tsp = transpose' board
    in (rowWins board drawn) || (rowWins tsp drawn)

readAllBoards :: [String] -> [[Int]]
readAllBoards [] = []
readAllBoards strs = (readBoard strs):(readAllBoards $ drop 6 strs)

calcScore :: [Int] -> [Int] -> Int -> Int
calcScore board rands ind =
    let vals = take ind rands
        rem = filter (\x -> notElem x vals) board
    in (sum rem) * (last vals)

getFirstScore :: [[Int]] -> [Int] -> Int -> Int
getFirstScore boards rands ind =
    let fin = filter (boardWins (take ind rands)) boards
    in
        if (length fin == 0) then
            getFirstScore boards rands (ind+1)
        else
            calcScore (head fin) rands ind

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        rands = map (read :: String -> Int) $ splitOn (==',') $ head lns
        allBoards = readAllBoards $ drop 2 lns
    putStrLn $ show $ getFirstScore allBoards rands 1

