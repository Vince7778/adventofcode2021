import Data.Char (digitToInt)
import Data.Bifunctor ( Bifunctor(second) )
import Data.List (sortBy)

neighbors :: (Num a1, Num a2) => (a1, a2) -> [(a1, a2)]
neighbors (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

valTuple :: [String] -> (Int, Int) -> Char
valTuple lns (x,y)
    | x < 0 || x >= length lns = '9'
    | y < 0 || y >= length (head lns) = '9'
    | otherwise = (lns !! x) !! y

tupleArr :: Int -> Int -> [(Int, Int)]
tupleArr a b = [ (x, y) | x <- [0..(a-1)], y <- [0..(b-1)] ]

addTuples :: (Int -> a -> a) -> [String] -> ([(Int, Int)], a) -> (Int, Int) -> ([(Int, Int)], a)
addTuples func lns (avis, acc) x = (fst res, func (snd res) acc)
    where res = floodfill lns avis x

floodfill :: [String] -> [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], Int)
floodfill lns vis pos
    | pos `elem` vis = (vis, 0)
    | valTuple lns pos == '9' = (vis, 0)
    | otherwise = foldl (addTuples (+) lns) (pos:vis, 1) $ neighbors pos

floodfillAll :: [String] -> [Int]
floodfillAll lns = snd $ foldl (addTuples (:) lns) ([], []) tarr
    where tarr = tupleArr (length lns) (length $ head lns)

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        sizes = sortBy (flip compare) $ filter (>0) $ floodfillAll lns
        ans = product $ take 3 sizes
    print ans
