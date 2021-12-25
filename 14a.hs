import Utils (splitOn, removeNth, listToPair)
import qualified Data.Map as Map
import qualified Data.Bifunctor

letterMap :: Map.Map Char Int
letterMap = Map.fromList $ zip "CNBHKOSPVF" [0..]

type PairCount = [((Int, Int), Int)]

countPairs :: [Int] -> PairCount
countPairs str = Map.toList $ Map.fromListWith (+) [(x, 1) | x <- pairs]
    where pairs = zip <*> tail $ str

insertPairs :: Map.Map (Int, Int) Int -> PairCount -> PairCount
insertPairs _ [] = []
insertPairs lmap ((pair, count):xs) = ((fst pair, val), count) : ((val, snd pair), count) : insertPairs lmap xs
    where val = Map.findWithDefault (negate 1) pair lmap

runStep :: Map.Map (Int, Int) Int -> PairCount -> PairCount
runStep lmap pcnt = Map.toList $ Map.fromListWith (+) $ insertPairs lmap pcnt

main' :: [String] -> String
main' lns =
    let str = head lns
        pairs = map (listToPair . removeNth 1 . splitOn ' ') $ drop 2 lns
        mapFind = map (\y -> Map.findWithDefault (-1) y letterMap)
        numStr = mapFind str
        pairCounts = countPairs numStr
        numPairs = map (Data.Bifunctor.bimap (listToPair . mapFind) (head . mapFind)) pairs
        lmap = Map.fromList numPairs
        stepped = iterate (runStep lmap) pairCounts !! 10
        seconds = map (Data.Bifunctor.first snd) stepped
        secondMap = Map.fromListWith (+) ((head numStr, 1):seconds)
        finalCounts = Map.elems secondMap
        ans = maximum finalCounts - minimum finalCounts
    in show ans

main :: IO ()
main = do
    content <- getContents
    putStrLn $ main' $ lines content