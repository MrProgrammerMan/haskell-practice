-- a
and :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (True:ps) = Main.and ps

-- b
concatAlt :: [[a]] -> [a]
concatAlt [] = []
concatAlt ([]:ls) = concatAlt ls
concatAlt ((x:xs):ls) = x : concatAlt (xs:ls)

concatAlt2 :: [[a]] -> [a]
concatAlt2 [] = []
concatAlt2 (l:ls) = l ++ concatAlt2 ls

-- c
replicateAlt :: Int -> a -> [a]
replicateAlt 0 _ = []
replicateAlt n x = x : replicateAlt (n-1) x

-- d
(!!!) :: [a] -> Int -> a
(l:_) !!! 0 = l
(_:ls) !!! n = ls !!! (n-1)


-- e
elemAlt :: Eq a => a -> [a] -> Bool
elemAlt _ [] = False
elemAlt a (l:ls) = if a == l then True else elemAlt a ls