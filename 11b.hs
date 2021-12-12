import Data.Char (digitToInt)

type Point = (Int, Int)
type Board = [[Int]]

neighbors :: Point -> [Point]
neighbors (x,y) = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]

valTuple :: Board -> Point -> Int
valTuple lns (x,y)
    | x < 0 || x >= length lns = 0
    | y < 0 || y >= length (head lns) = 0
    | otherwise = lns !! x !! y

tupleArr :: Int -> Int -> [Point]
tupleArr a b = [ (x, y) | x <- [0..(a-1)], y <- [0..(b-1)] ]

arr10 = tupleArr 10 10

funcInd :: [a] -> Int -> (a -> a) -> [a]
funcInd [] _ _ = []
funcInd (x:xs) 0 func = func x:xs
funcInd (x:xs) ind func = x:funcInd xs (ind-1) func

func2Ind :: [[a]] -> Point -> (a -> a) -> [[a]]
func2Ind xs (x,y) func =
    let row = xs !! x
        modRow = funcInd row y func
    in funcInd xs x (const modRow)

combineB :: (Board, Int) -> Point -> (Board, Int)
combineB acc x = (fst res, snd res + snd acc)
    where res = recurse (fst acc) x

incNonzero :: Int -> Int
incNonzero x = if x == 0 then 0 else x+1

recurse :: Board -> Point -> (Board, Int)
recurse b pt
    | valTuple b pt < 10 = (b, 0)
    | otherwise =
        let neighb = neighbors pt
            added = foldl (\acc x -> func2Ind acc x incNonzero) b neighb
            setZero = func2Ind added pt (const 0)
        in foldl combineB (setZero, 1) neighb

runStep :: Board -> (Board, Int)
runStep b = foldl combineB (inc, 0) arr10
    where inc = map (map (+1)) b

runAll :: Board -> Int
runAll b = if snd stepped == 100 then 1 else 1 + runAll (fst stepped)
    where stepped = runStep b
        

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        energies = map (map digitToInt) lns :: Board
        ans = runAll energies
    print ans
