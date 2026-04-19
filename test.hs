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