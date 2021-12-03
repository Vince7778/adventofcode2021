

stringLToInt :: [String] -> [Int]
stringLToInt = map read

tuplify :: String -> (String, Int)
tuplify s =
    let w = words s
    in (head w, read $ last w)

parseDir :: (String, Int) -> (Int, Int)
parseDir ("forward", v) = (v, 0)
parseDir ("down", v) = (0, v)
parseDir ("up", v) = (0, (-v))

addParse :: (Int, Int) -> (String, Int) -> (Int, Int)
addParse (x, y) t =
    let ps = parseDir t
    in (x + fst ps, y + snd ps)

finalPos :: [(String, Int)] -> (Int, Int)
finalPos ts = foldl addParse (0, 0) ts

main :: IO()
main = do
    content <- getContents
    let strList = lines content
        tuples = map tuplify strList
        fpos = finalPos tuples
        fprod = (fst fpos) * (snd fpos)
    putStrLn $ show fprod


