module Utils
( funcInd
, func2Ind
, splitOn
, valTuple
, removeNth
, listToPair
) where
import Data.List ( groupBy )
import Data.Function ( on )

splitOn :: Char -> String -> [String]
splitOn ch s =
    let grouped = groupBy ((==) `on` (==ch)) s
    in filter (/=[ch]) grouped

funcInd :: [a] -> Int -> (a -> a) -> [a]
funcInd [] _ _ = []
funcInd (x:xs) 0 func = func x:xs
funcInd (x:xs) ind func = x:funcInd xs (ind-1) func

func2Ind :: [[a]] -> (Int, Int) -> (a -> a) -> [[a]]
func2Ind xs (x,y) func =
    let row = xs !! x
        modRow = funcInd row y func
    in funcInd xs x (const modRow)

valTuple :: [[a]] -> (Int, Int) -> Maybe a
valTuple [] _ = Nothing
valTuple arr (x,y)
    | x < 0 || x >= length arr = Nothing
    | y < 0 || y >= length (head arr) = Nothing
    | otherwise = Just (arr !! x !! y)

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth 0 (x:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) xs

listToPair :: [a] -> (a, a)
listToPair lst
    | length lst < 2 = error "Length of list is less than 2"
    | otherwise = (head lst, lst !! 1)