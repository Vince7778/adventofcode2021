import Data.List (permutations, elemIndex, sort, find)
import Data.Char (ord)
import Data.Maybe

segList = 
    [ [0,1,2,4,5,6]
    , [2,5]
    , [0,2,3,4,6]
    , [0,2,3,5,6]
    , [1,2,3,5]
    , [0,1,3,5,6]
    , [0,1,3,4,5,6]
    , [0,2,5]
    , [0,1,2,3,4,5,6]
    , [0,1,2,3,5,6] ]

parseLine :: String -> ([String], [String])
parseLine s = (take 10 wds, drop 11 wds)
    where wds = words s

parseSegs :: String -> [Int]
parseSegs = map (\x -> ord x - ord 'a')

mapPerm :: [Int] -> [Int] -> Int
mapPerm perm str =
    let mappedStrs = sort $ map (\x -> perm !! x) str
        mapFind = elemIndex mappedStrs segList
    in fromMaybe (-1) mapFind

validPerm :: [[Int]] -> [Int] -> Bool
validPerm strs perm = sort (map (mapPerm perm) strs) == [0..9]

checkPerms :: [[Int]] -> [Int]
checkPerms strs = fromMaybe [] $ find (validPerm strs) $ permutations [0..6]

calcVal :: [[Int]] -> [Int] -> Int
calcVal strs perm = foldl (\acc x -> 10*acc + x) 0 $ map (mapPerm perm) strs

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        parsed = map parseLine lns
        segged = map (\x -> (map parseSegs (fst x), map parseSegs (snd x))) parsed
        permed = map (\x -> calcVal (snd x) $ checkPerms (fst x)) segged
        ans = sum permed
    putStrLn $ show ans

