import Numeric (readHex)
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)
import Debug.Trace (trace)

hexToBin :: Char -> Maybe String
hexToBin c = case readHex [c] of
    (x,_):_ -> Just $ printf "%04b" (x::Int)
    _       -> Nothing

binToDec :: String -> Int
binToDec = foldl (\acc x -> 2*acc+digitToInt x) 0

doOper :: Int -> [Int] -> Int
doOper v x | trace ("oper " ++ show v ++ " on " ++ show x) False = undefined
doOper 0 x = sum x
doOper 1 x = product x
doOper 2 x = minimum x
doOper 3 x = maximum x
doOper 5 x = fromEnum $ head x > last x
doOper 6 x = fromEnum $ head x < last x
doOper 7 x = fromEnum $ head x == last x

parseType0 :: String -> [Int]
parseType0 [] = []
parseType0 p = snd ret : parseType0 (fst ret)
    where ret = parsePacket p

parseType1 :: String -> Int -> (String, [Int])
parseType1 p 0 = (p, [])
parseType1 p rem =
    let pack = parsePacket p
        ret = parseType1 (fst pack) (rem-1)
    in (fst ret, snd pack : snd ret)

parseOper :: String -> (String, [Int])
parseOper p =
    let lenTyp = head p == '0'
        p' = tail p
    in if lenTyp then
        let len = binToDec $ take 15 p'
            dropped = drop 15 p'
        in (drop len dropped, parseType0 $ take len dropped)
    else
        let num = binToDec $ take 11 p'
        in parseType1 (drop 11 p') num

parseLit :: String -> (String, String)
parseLit p =
    let typ = head p == '1'
        bin = take 4 $ tail p
    in if not typ then (drop 5 p, bin) else
        let ret = parseLit $ drop 5 p
        in (fst ret, bin ++ snd ret)

parsePacket :: String -> (String, Int)
parsePacket p =
    let ver = binToDec $ take 3 p
        typ = binToDec $ take 3 $ drop 3 p
        rest = drop 6 p
    in if typ == 4 then 
        let ret = parseLit rest
        in (fst ret, binToDec $ snd ret)
    else 
        let ret = parseOper rest
        in (fst ret, doOper typ (snd ret))

main = do
    content <- getContents
    let mpd = mapMaybe hexToBin content
        conc = concat mpd
        ans = parsePacket conc
    print conc
    print ans