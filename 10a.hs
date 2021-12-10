import Data.Map ( Map, member )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )

parenMatch :: Map Char Char
parenMatch = Map.fromList $ zip "([{<" ")]}>"

parenVals :: Map Char Int
parenVals = Map.fromList $ zip ")]}>" [3, 57, 1197, 25137]

corrupted :: String -> [Char] -> Maybe Char
corrupted [] _ = Nothing
corrupted (x:xs) [] = corrupted xs [x]
corrupted (x:xs) a@(top:stk)
    | member x parenMatch = corrupted xs (x:a)
    | otherwise = case Map.lookup top parenMatch of
        Nothing -> Nothing
        Just close -> if close /= x then Just x else corrupted xs stk

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        corrs = mapMaybe (`corrupted` []) lns
        ans = sum $ mapMaybe (`Map.lookup` parenVals) corrs
    print ans

