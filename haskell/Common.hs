module Common
( primesTo
, leastFactor
, primeFactors
) where

import Data.List.Ordered (minus)

primesTo :: (Ord a, Num a, Enum a) => a -> [a]
primesTo n = 2 : eratos [3,5..n] where
    eratos [] = []
    eratos (x:xs) = x : eratos (xs `minus` [x*x, x*x+2*x..n])

smallPrimes = primesTo 100000

leastFactor :: Integer -> Integer
leastFactor 1 = 1
leastFactor x = head [y | y <- (takeWhile (<= x) smallPrimes), x `mod` y == 0]

primeFactors :: Integer -> [Integer]
primeFactors x = (takeWhile (<=x) (facts x [])) where
    facts 1 [] = []
    facts 1 (y:ys) = y:ys
    facts m lst = facts (m `div` (leastFactor m)) ((leastFactor m):lst)

