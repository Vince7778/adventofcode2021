import Data.Char (digitToInt)

neighbors (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

valTuple :: [String] -> (Int, Int) -> Char
valTuple lns (x,y)
    | x < 0 || x >= length lns = '9'
    | y < 0 || y >= length (head lns) = '9'
    | otherwise = (lns !! x) !! y

tupleArr :: Int -> Int -> [(Int, Int)]
tupleArr a b = [ (x, y) | x <- [0..(a-1)], y <- [0..(b-1)] ]

lowNeighbors :: [String] -> (Int, Int) -> Bool
lowNeighbors lns tup = all (> valTuple lns tup) $ map (valTuple lns) $ neighbors tup

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        tarr = tupleArr (length lns) (length $ head lns)
        filt = filter (lowNeighbors lns) tarr
        vals = map (\x -> digitToInt (valTuple lns x)+1) filt
        ans = sum vals
    putStrLn $ show ans
