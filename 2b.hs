

stringLToInt :: [String] -> [Int]
stringLToInt = map read

tuplify :: String -> (String, Int)
tuplify s =
    let w = words s
    in (head w, read $ last w)

parseDir :: (String, Int) -> Int -> (Int, Int, Int)
parseDir ("forward", v) aim = (v, v*aim, aim)
parseDir ("down", v) aim = (0, 0, aim+v)
parseDir ("up", v) aim = (0, 0, aim-v)

addParse :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
addParse (x, y, aim) t =
    let (pa, pb, pc) = parseDir t aim
    in (x + pa, y + pb, pc)

finalPos :: [(String, Int)] -> (Int, Int, Int)
finalPos = foldl addParse (0, 0, 0)

main :: IO()
main = do
    content <- getContents
    let strList = lines content
        tuples = map tuplify strList
        (fpa, fpb, fpaim) = finalPos tuples
        fprod = fpa * fpb
    putStrLn $ show fprod


