-- Advanced Programming, HW 1
-- by Steven Tomcavage

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude hiding (all, reverse, takeWhile, zip)
import Test.HUnit 

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ test0, test1, test2, test3 ]
   return ()

-- Part 0 (a)

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = if x && (y || z) then True else False  
 
 
t0a :: Test
t0a = "0a1" ~: TestList [abc True False True ~?= True, 
                         abc True False False ~?= False,
                         abc False True True ~?= False]

-- 0 (b)

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
        (((b*f) - (c*e)), ((c* d) - (a*f)), ((a*e)-(b*d)))
         

t0b :: Test
t0b = "0b" ~: TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
                        arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- 0 (c)

cmax :: [Int] -> Int -> Int
cmax [] t = t
cmax l t  = max (maximum l) t
                

t0c :: Test
t0c ="0c" ~: TestList[ cmax [1,4,2] 0 ~?= 4, 
                       cmax []      0 ~?= 0,
                       cmax [5,1,5] 0 ~?= 5 ]

-- 0 (d)

reverse :: [a] -> [a] 
reverse []     = [] 
reverse (l:ls) = (reverse ls) ++ [l]

t0d :: Test
t0d = "0d" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                        reverse [1]     ~?= [1] ]

test0 :: Test
test0 = "test0" ~: TestList [ t0a , t0b, t0c, t0d ]


-- Part 1 (a)  

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n >= 10 = (n `rem` 10) : (toDigitsRev (n `quot` 10))
              | n < 10  = [n] 
toDigitsRev _ = []

t1a :: Test
t1a = "1a" ~: TestList [ toDigitsRev 1234 ~?= [4,3,2,1],
                         toDigitsRev 0    ~?= [0] ]                       

-- 1 (b)  

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther []       = []
doubleEveryOther (x:y:ys) = x : (y + y) : (doubleEveryOther ys)
doubleEveryOther (x:_)    = [x]

t1b :: Test
t1b = "1b" ~: TestList [ doubleEveryOther [8,7,6,5] ~?= [8,14,6,10],
                         doubleEveryOther [8,7,6,5,4] ~?= [8,14,6,10,4] ]

-- 1 (c) 

sumDigits :: [Integer] -> Integer
sumDigits []                 = 0
sumDigits (n:ns) | n < 10    = n + (sumDigits ns)
                 | otherwise = (sumDigits (toDigits n)) + (sumDigits ns)

t1c :: Test
t1c = "1c" ~: sumDigits[8,14,6,10] ~?= 20

-- 1 (d) 

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

t1d :: Test
t1d = "1d" ~: validate 4012888888881881 ~?= True

test1 :: Test
test1 = TestList [ t1a, t1b, t1c, t1d ]

-- 2a)

-- | The conv function takes two lists of numbers, reverses the 
-- second list, multiplies their elements together pointwise, and sums
-- the result.  This function assumes that the two input
-- lists are the same length.
 
conv :: [Int] -> [Int] -> Int
conv [] [] = 0
conv xs ys = ((head xs) * (last ys)) + (conv (tail xs) (init ys))

t2a :: Test
t2a = "2a" ~: conv [2,4,6] [1,2,3] ~?= 20

-- 2b) 

-- | The normalize function adds extra zeros to the beginning of each
-- list so that they each have length 2n + 1, where n is 
-- the length of the longer number.   
 
normalize :: [Int] -> [Int] -> ([Int], [Int])
normalize xs ys = (normalizedX, normalizedY) where 
        lenX = length xs
        lenY = length ys
        size = max lenX lenY * 2 + 1 -- 2n + 1 where n is the longest list size
        normalizedX = normalize' xs (size - lenX)
        normalizedY = normalize' ys (size - lenY)
        normalize' zs count 
            | count == 0 = zs
            | otherwise  = normalize' (0:zs) (count - 1) 
         

t2b :: Test
t2b = "2b" ~: normalize [1] [2,3] ~?= ([0,0,0,0,1], [0,0,0,2,3])

-- 2c)

-- | multiply two numbers, expressed as lists of digits using 
-- the Åªrdhva TiryagbhyÄ�m algorithm.
 
-- Because this multiply algorithm operates on successively larger sub-lists
-- starting with the end of each input list, I chose to reverse my lists
-- before multiplying them to make extracting the sub-lists easier.
multiply :: [Int] -> [Int] -> [Int]
multiply xs ys = reverse (multiply' 0 1) where
    (normXs, normYs) = normalize xs ys
    revXs = reverse normXs
    revYs = reverse normYs
    maxCount = max (length revXs) (length revYs)
    multiply' c count 
        | count > maxCount = []
        | otherwise        = z1:(multiply' c1 (count + 1)) where
            s1 = conv (take count revXs) (take count revYs)
            z1 = (s1 + c) `mod` 10
            c1 = (s1 + c) `div` 10      

t2c :: Test
t2c = "2c" ~: multiply [2,4,6][1,2,3] ~?= [0,0,3,0,2,5,8]

-- 2d) OPTIONAL CHALLENGE PROBLEM 

convAlt :: [Int] -> [Int] -> Int
convAlt = error "unimplemented"

t2d :: Test
t2d = "2d" ~: convAlt [2,4,6][1,2,3] ~=? 20

test2 :: Test
test2 = TestList [t2a,t2b,t2c,t2d]

test3 :: Test
test3 = "test3" ~: TestList [t3a, t3b, t3c, t3d, t3e, t3f, t3g, t3h]

-- 3 (a)

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse _ []                     = []
intersperse e (l:ls) | null ls       = [l]
                     | not (null ls) = l:[e] ++ (intersperse e ls)
intersperse _ _                      = []

t3a :: Test
t3a = "3a" ~: TestList [ intersperse ',' "abcde" ~?= "a,b,c,d,e",
                         intersperse ',' "ab"    ~?= "a,b",
                         intersperse ',' "a"     ~?= "a", 
                         intersperse ',' ""      ~?= "" ]


-- 3 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert []         = []
invert ((a,b):xs) = (b, a) : (invert xs)

t3b :: Test
t3b = "3b" ~: TestList [ invert [("a",1),("b",2)] ~?= [(1,"a"),(2,"b")],
                         invert [("a",1)]         ~?= [(1,"a")] ]
 

-- 3 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []                   = []
takeWhile f (x:xs) | f x == True = x : (takeWhile f xs)
                   | otherwise   = []

t3c :: Test
t3c = "3c" ~: TestList [ takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
                         takeWhile (< 9) [1,2,3]           ~?= [1,2,3],
                         takeWhile (< 0) [1,2,3]           ~?= [] ] 

-- 3 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find _ []                   = Nothing
find f (x:xs) | f x == True = Just x
              | otherwise   = find f xs

t3d :: Test
t3d = "3d" ~: TestList [ find odd []        ~?= Nothing,
                         find odd [2,4,6]   ~?= Nothing, 
                         find odd [0,2,3,4] ~?= Just 3,
                         find odd [0,2,3,5] ~?= Just 3 ]
 

-- 3 (e)

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a -> Bool) -> [a] -> Bool
all _ []                    = True
all f (x:xs) | f x == False = False
             | otherwise    = all f xs

t3e :: Test
t3e = "3e" ~: TestList [ all odd []      ~?= True,
                         all odd [1,2,3] ~?= False,
                         all odd [1,3,5] ~?= True ]
 

-- 3 (f)

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _          = []
map2 _ _ []          = []
map2 f (x:xs) (y:ys) = (f x y) : (map2 f xs ys) 


t3f :: Test
t3f = "3f" ~: TestList [ map2 max [1,2,3] [3,2,1] ~?= [3,2,3],
                         map2 (\a b -> a + b) [1,2,3] [3,2,1,0] ~?= [4, 4, 4],
                         map2 (\a b -> a + b) [1,2,3,0] [3,2,1] ~?= [4, 4, 4] ]

-- 3 (g)

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]

zip :: [a] -> [b] -> [(a,b)]
zip _ []          = []
zip [] _          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

t3g :: Test
t3g = "3g" ~: TestList [ zip [1, 2, 3] "abc" ~?= [(1,'a'), (2,'b'), (3,'c')],
                         zip [1, 2, 3] "ab"  ~?= [(1,'a'), (2,'b')],
                         zip [1, 2] "abc"    ~?= [(1,'a'), (2,'b')] ]

-- 3 (h)  WARNING this one is tricky!

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]

transpose :: [[a]] -> [[a]]
transpose (x:(y:[])) = map2 (\ a b -> a : b : []) x y
transpose _          = []

t3h :: Test
t3h = "3h" ~: TestList [transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
                        transpose [[1,2,3,4],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
                        transpose [[1,2,3],[4,5,6,7]] ~?= [[1,4],[2,5],[3,6]]]
                         