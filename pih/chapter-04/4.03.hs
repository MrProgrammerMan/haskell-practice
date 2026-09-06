-- a
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b
safetail2 :: [a] -> [a]
safetail2 xs
    | null xs = []
    | otherwise = tail xs

-- c
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs