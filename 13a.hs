import Utils (splitOn, func2Ind)
import Data.List (foldl', transpose)

foldx :: [[Bool]] -> Int -> [[Bool]]
foldx arr x = map (\r -> take x $ zipWith (||) r $ reverse r) narr
    where narr = map (take (x*2+1)) arr

foldy :: [[Bool]] -> Int -> [[Bool]]
foldy arr y = transpose $ foldx (transpose arr) y

main :: IO()
main = do
    content <- getContents
    let lns = lines content
        pts = map (map read . splitOn ',') $ takeWhile (/= []) lns :: [[Int]]
        insts = take 1 $ map (drop 11) $ drop (1+length pts) lns
        startArr = replicate 896 (replicate 1312 False)
        added = foldl' (\acc x -> func2Ind acc (last x, head x) (const True)) startArr pts
        xinsts = map (read . drop 2) $ filter (\x -> head x == 'x') insts :: [Int]
        yinsts = map (read . drop 2) $ filter (\x -> head x == 'y') insts :: [Int]
        f1 = foldl' foldx added xinsts
        f2 = foldl' foldy f1 yinsts
        total = sum $ map (sum . map fromEnum) f2
    print total
