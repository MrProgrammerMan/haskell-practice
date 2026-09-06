safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- a
third :: [a] -> Maybe a
third xs = safeTail xs >>= safeTail >>= safeHead -- This is the better way(Maybe is a monad). safeTail can fail, so it returns the monad m. That means the result can be composed(unpacked and stuffed) into safeTail again with >>=. Again for safeHead. Crazy stuff...
-- My own solution composing a safe `third`: third = (liftArgMaybe safeHead) . (liftArgMaybe safeTail) . safeTail
-- Old solution, causes warning: third = head . tail . tail

--b (partial??? throws on lists shorter than 3)
third2 :: [a] -> a
third2 ls = ls !! 2

--c (partial)
third3 :: [a] -> a
third3 (_:_:x:_) = x