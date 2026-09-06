positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (i, x) <- zip [0..] xs, x == n]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 n xs = find n (zip xs [0..])