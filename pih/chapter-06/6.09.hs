-- a
sumAlt :: Num a => [a] -> a
sumAlt [] = 0
sumAlt (x:xs) = x + sum xs

-- b
takeAlt :: Int -> [a] -> [a]
takeAlt 0 _ = []
takeAlt n (l:ls) = l : takeAlt (n-1) ls

-- c
lastAlt2 :: [a] -> a
lastAlt2 [x] = x
lastAlt2 (_:l) = lastAlt2 l