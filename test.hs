import Data.Char

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
init [l] = []
init (l:ls) = l : Main.init ls
-- Old solution, produces warning: init ls = reverse (tail (reverse ls))

init2 [l] = []
init2 (l:ls) = l : init2 ls
-- Old solution, produces warning: init2 = reverse . tail . reverse

init3 [l] = []
init3 (l:ls) = l : init3 ls
-- Old solution, produces warnin: init3 = reverse . (drop 1) . reverse

initAlt [x] = []
initAlt (x:xs) = x : initAlt xs

-- 3.1
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

-- 3.4
-- Checked

-- 3.5
-- For most types a, there are infinitely many functions that return an a. To check this equality, you would need to check every possible argument. In the case that the return type is a singleton, or the parameter type is simple enough to check all inputs, you could technically define equality.

-- 4.1
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2

-- 4.2
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

-- 4.3
-- a
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b
safetail2 :: [a] -> [a]
safetail2 xs
    | null xs = []
    | otherwise = tail xs

-- c
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

-- 4.4
(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
False |||| b = b

(|||||) :: Bool -> Bool -> Bool
False ||||| False = False
True ||||| True = True
True ||||| False = True
False ||||| True = True

(||||||) :: Bool -> Bool -> Bool
b |||||| c
    | b == False && c == False = False
    | otherwise = True

-- 4.5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then if b then True else False else False

-- 4.6
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a then b else False

-- 4.7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x*y*z

-- 4.8
luhnDouble :: Int -> Int
luhnDouble x
    | x > 4 = x*2-9
    | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

-- 5.1
fiveOne = sum [x*x | x <- [1..100]]

-- 5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 5.3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 5.4
replicate :: Int -> a -> [a]
replicate n val = [val | _ <- [1..n]]

-- 5.5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], 2*x == sum (factors x)]

-- 5.7
fiveSevenRef :: [(Int,Int)]
fiveSevenRef = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 5.8
positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (i, x) <- zip [0..] xs, x == n]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 n xs = find n (zip xs [0..])

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 5.10
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

let2intUpper :: Char -> Int
let2intUpper c = ord c - ord 'A'

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]