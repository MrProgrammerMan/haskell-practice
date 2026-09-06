a = ['a','b','c'] :: [Char]
b = ('a','b','c') :: (Char,Char,Char)
c = [(False,'0'), (True,'1')] :: [(Bool,Char)]
d = ([False,True], ['0','1']) :: ([Bool],[Char])
e = [safeTail,liftMaybe Prelude.init,liftMaybe reverse] :: [[a] -> Maybe [a]]
-- Old solution, produces warning: e = [tail,Prelude.init,reverse] :: [[a] -> [a]]

-- Used to get rid of warnings in the above task
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

-- Used to get rid of warnings in the above task
liftMaybe :: (a -> b) -> (a -> Maybe b)
liftMaybe f = Just . f