qsort [] = []
qsort (x:xs) = qsort bigger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        bigger = [b | b <- xs, b > x]