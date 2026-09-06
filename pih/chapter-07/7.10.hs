altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (l:ls) = f l : altMap g f ls

luhnDouble :: Int -> Int
luhnDouble x
    | x > 4 = x*2-9
    | otherwise = x*2

luhn' :: [Int] -> Bool
luhn' = (==0) . (`mod` 10) . sum . (altMap luhnDouble id)