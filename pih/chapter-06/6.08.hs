mergeAlt :: Ord a => [a] -> [a] -> [a]
mergeAlt [] l = l
mergeAlt l [] = l
mergeAlt (x:xs) (y:ys) = if x < y
    then x : mergeAlt xs (y:ys)
    else y : mergeAlt (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = mergeAlt (msort xs) (msort ys)
    where (xs,ys) = split l

split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:l) = (x : fst (split l), y : snd (split l))