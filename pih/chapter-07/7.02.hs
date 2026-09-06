-- a
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = if p x then all' p xs else False

all'' :: (a -> Bool) -> [a] -> Bool
all'' p = foldr (\x acc -> p x && acc) True

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x acc -> p x || acc) False

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr (\x acc -> if p x then x : acc else []) []

-- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs)