dec2int :: [Int] -> Int
dec2int = foldl (\x acc -> x * 10 + acc) 0