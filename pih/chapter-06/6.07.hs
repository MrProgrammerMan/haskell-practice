mergeAlt :: Ord a => [a] -> [a] -> [a]
mergeAlt [] l = l
mergeAlt l [] = l
mergeAlt (x:xs) (y:ys) = if x < y
    then x : mergeAlt xs (y:ys)
    else y : mergeAlt (x:xs) ys