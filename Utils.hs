module Utils
( funcInd
, func2Ind
, splitOn
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