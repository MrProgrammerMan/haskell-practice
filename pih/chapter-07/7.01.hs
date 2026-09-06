process xs f p = [f x | x <- xs, p x]
processAlt xs f p = map f (filter p xs)
processPointFree = \f p -> map f . filter p