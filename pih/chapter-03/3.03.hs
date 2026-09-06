safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

second :: [a] -> Maybe a
second = (liftArgMaybe safeHead) . (safeTail)
-- Old solution, produces warning: second xs = head (tail xs)

-- Used to get rid of warnings in the above task
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Used to get rid of warnings in the above task
liftArgMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
liftArgMaybe _ Nothing = Nothing
liftArgMaybe f (Just x) = f x

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double2 :: Num a => a -> a
double2 x = x*2

palindrome :: [Char] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)