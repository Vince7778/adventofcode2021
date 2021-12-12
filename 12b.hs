import qualified Data.Map as Map
import Utils (splitOn)
import Data.List (foldl')
import Data.Char (isLower)

type AdjMap = Map.Map String [String]

insMap :: String -> String -> AdjMap -> AdjMap
insMap k v mp
    | Map.notMember k mp = Map.insert k [v] mp
    | otherwise = Map.adjust (v:) k mp

edgeAdd :: AdjMap -> String -> AdjMap
edgeAdd mp str =
    let splt = splitOn '-' str
        func v xs = Just (v:xs)
        ins1 = insMap (head splt) (last splt) mp
    in insMap (last splt) (head splt) ins1

getAdjList :: [String] -> AdjMap
getAdjList = foldl' edgeAdd Map.empty

isSmall :: String -> Bool
isSmall s = isLower $ head s

dfs :: AdjMap -> Map.Map String Bool -> Bool -> String -> Int
dfs adj vis used x 
    | x == "end" = 1
    | x == "start" && Map.findWithDefault True "start" vis = 0
    | isSmall x && Map.findWithDefault True x vis && used = 0
    | otherwise =
        let updMap = Map.insert x True vis
            nbrs = Map.findWithDefault [] x adj
            newUsed = used || (isSmall x && Map.findWithDefault True x vis)
        in sum $ map (dfs adj updMap newUsed) nbrs

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        adjList = getAdjList lns
        visList = Map.fromList $ zip (Map.keys adjList) (repeat False)
        ans = dfs adjList visList False "start"
    print ans

