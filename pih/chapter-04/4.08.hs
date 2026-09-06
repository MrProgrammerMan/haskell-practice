luhnDouble :: Int -> Int
luhnDouble x
    | x > 4 = x*2-9
    | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0