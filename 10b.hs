import Data.Map ( Map, member )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import Data.List (sort)

parenMatch :: Map Char Char
parenMatch = Map.fromList $ zip "([{<" ")]}>"

parenVals :: Map Char Int
parenVals = Map.fromList $ zip "([{<" [1, 2, 3, 4]

incomplete :: String -> [Char] -> Maybe [Char]
incomplete [] stk = Just stk
incomplete (x:xs) [] = incomplete xs [x]
incomplete (x:xs) a@(top:stk)
    | member x parenMatch = incomplete xs (x:a)
    | otherwise = case Map.lookup top parenMatch of
        Nothing -> Nothing
        Just close -> if close /= x then Nothing else incomplete xs stk

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        incomps = mapMaybe (`incomplete` []) lns
        mapped = map (mapMaybe (`Map.lookup` parenVals)) incomps
        sorted = sort $ map (foldl (\acc x -> acc*5+x) 0) mapped
        ans = sorted !! (length sorted `div` 2)
    print ans

