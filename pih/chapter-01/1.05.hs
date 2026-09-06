qsortUniq [] = []
qsortUniq (x:xs) = qsortUniq smaller ++ [x] ++ qsortUniq bigger
    where
        smaller = [a | a <- xs, a < x]
        bigger = [b | b <- xs, b > x]
-- duplicates are removed