-- Excercises

-- 1.1
-- double (double 2)
-- double (2 + 2)
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8

-- 1.2
-- sum [x]
-- = sum x : []
-- = x + sum []
-- = x + 0
-- = 0

-- 1.3
product [] = 1
product (n:ns) = n * Main.product ns
-- product [2,3,4]
-- = 2 * product [3,4]
-- = 2 * (3 * product [4])
-- = 2 * (3 * (4 * product []))
-- = 2 * (3 * (4 * 1))
-- = 24

-- 1.4
qsort [] = []
qsort (x:xs) = qsort bigger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        bigger = [b | b <- xs, b > x]

-- 1.5
qsortUniq [] = []
qsortUniq (x:xs) = qsortUniq smaller ++ [x] ++ qsortUniq bigger
    where
        smaller = [a | a <- xs, a < x]
        bigger = [b | b <- xs, b > x]
-- duplicates are removed

--

-- 2.1
double x = x + x

quadruple x = double (double x)

quadrupleAlt = double . double

factorial n = Main.product [1..n]

-- 2.2
-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

-- 2.3
n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

-- 2.4
last ls = (reverse ls) !! 0

lastAlt ls = ls !! (length ls - 1)

-- 2.5
init ls = reverse (tail (reverse ls))

init2 = reverse . tail . reverse

init3 = reverse . (drop 1) . reverse

initAlt [x] = []
initAlt (x:xs) = x : initAlt xs

-- 3.1
a = ['a','b','c'] :: [Char]
b = ('a','b','c') :: (Char,Char,Char)
c = [(False,'0'), (True,'1')] :: [(Bool,Char)]
d = ([False,True], ['0','1']) :: ([Bool],[Char])
e = [tail,Prelude.init,reverse] :: [[a] -> [a]]

-- 3.2
bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[1,2,3],[4,7],[88,9]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.3
second :: [a] -> a
second xs = head (tail xs)

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

-- 3.4
-- Checked

-- 3.5
-- For most types a, there are infinitely many functions that return an a. To check this equality, you would need to check every possible argument. In the case that the return type is a singleton, or the parameter type is simple enough to check all inputs, you could technically define equality.
