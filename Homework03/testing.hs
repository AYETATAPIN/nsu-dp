module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where
import Control.Concurrent (yield)


--task 1

unique :: Ord a => [a] -> Bool
unique [] = True
unique [first] = True
unique [first, second]
    | first == second = False
    | otherwise = True

unique (first:others)
    | first `elem` others = False
    | otherwise = unique others

--task 2
--bebra
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [1 .. ], y <- [1 .. z] , x <- [1 .. y], x*x + y*y == z*z]

--task 3
counting :: Integral a => a -> a -> a -> a
counting x y n
    | n == 1 = 0
    | (mod x n == 0) && (mod y n == 0) = 1
    | otherwise = counting x y (n - 1)
isBothPrime :: Integral a => a -> a -> a
isBothPrime 1 1 = 0
isBothPrime 2 1 = 0
isBothPrime 1 2 = 0
isBothPrime x y 
    | x == y = 0
    | x > y = counting x y y
    | otherwise = counting y x x

primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(x, y, z) | z <- [1 .. ], y <- [1 .. z] , x <- [1 .. y], x*x + y*y == z*z, isBothPrime x y == 0, isBothPrime x z == 0, isBothPrime y z == 0 ]

--task 4

sumDel :: Int -> Int -> Int
sumDel x n
  | x == n = 0
  | mod x n == 0 = n + sumDel x (n + 1)
  | otherwise = sumDel x (n + 1)

perfectNumbers :: Integral a => [a]
perfectNumbers = [ fromIntegral x | x <- [1 ..], sumDel x 1 == x]

--task 5 does it work?

countables :: Integral a => a -> [(a, a)]
countables n
    | n == 0 = [(0, 0)]
    | otherwise = [(x, y)]
    where w = x + y
        t = (w * (w + 1)) / 2
        w = floor((sqrt(8 * t + 1) - 1) / 2)
        y = n - t
        x = w - y

helpfulFunction :: Integral a => a -> [(a, a)]
helpfulFunction n = [countables n ++ helpfulFunction (n + 1)]

cantorPairs :: Integral a => [(a, a)]
cantorPairs = helpfulFunction 0

--task 6

minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance ((x1,y1): points) = min (minimum [sqrt ((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2)) | (x2,y2) <- points]) (minimalDistance points)

--task 7 absolutely no idea how to do it