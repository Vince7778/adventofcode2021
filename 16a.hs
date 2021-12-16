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
binToDec p | trace ("bruh1 " ++ show p) False = undefined
binToDec s= foldl (\acc x -> 2*acc+digitToInt x) 0 s

parseType0 :: String -> Int
parseType0 [] = 0
parseType0 p | trace ("bruh " ++ show p) False = undefined
parseType0 p = snd ret + parseType0 (fst ret)
    where ret = parsePacket p

parseType1 :: String -> Int -> (String, Int)
parseType1 p 0 = (p, 0)
parseType1 p rem =
    let pack = parsePacket p
        ret = parseType1 (fst pack) (rem-1)
    in (fst ret, snd pack + snd ret)

parseOper :: String -> (String, Int)
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

parseLit :: String -> String
parseLit p =
    let typ = head p == '1'
    in if typ then parseLit (drop 5 p) else drop 5 p

parsePacket :: String -> (String, Int)
parsePacket p =
    let ver = binToDec $ take 3 p
        typ = binToDec $ take 3 $ drop 3 p
        rest = drop 6 p
    in if typ == 4 then (parseLit rest, ver) else let ret = parseOper rest in (fst ret, snd ret + ver)

main = do
    content <- getContents
    let mpd = mapMaybe hexToBin content
        conc = concat mpd
        ans = parsePacket conc
    print conc
    print ans