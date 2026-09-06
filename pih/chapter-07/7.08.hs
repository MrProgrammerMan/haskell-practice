import Data.Char

unfold' p h t x | p x       = []
                | otherwise = h x : unfold' p h t (t x)

checkParities :: [[Bit]] -> [[Bit]]
checkParities = map checkParity

checkParity :: [Bit] -> [Bit]
checkParity (p:bits) | p == sum bits `mod` 2 = bits
                     | otherwise = error "Parity bit does not match. Impossible to decode"

encodeWithParity :: String -> [Bit]
encodeWithParity = concat . map (addParityBit . make8 . int2bin . ord)

addParityBit :: [Bit] -> [Bit]
addParityBit l = sum l `mod` 2 : l

decodeWithParity :: [Bit] -> String
decodeWithParity = map (chr . bin2int) . checkParities . chop9

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold' null (take 9) (drop 9)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold' null (take 8) (drop 8)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

faultyTransmitWithParity :: String -> String
faultyTransmitWithParity = decodeWithParity . faultyChannel . encodeWithParity