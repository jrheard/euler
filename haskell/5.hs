import Data.List (sort)
import Data.List.Ordered (minus, union)

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- so this works, but (head nums_divisible_by_1_through_20) doesn't terminate in less than a minute.
nums_divisible_by_1_through_20 = [x | x <- [1..], foldl (\acc y -> acc && (x `mod` y == 0)) True [1..20]]

primesTo :: (Ord a, Num a, Enum a) => a -> [a]
primesTo n = 2 : eratos [3,5..n] where
    eratos [] = []
    eratos (x:xs) = x : eratos (xs `minus` [x, x+2*x..n])

smallPrimes = primesTo 10000

leastFactor :: Integer -> Integer
leastFactor 1 = 1
leastFactor x = head [y | y <- (takeWhile (<= x) smallPrimes), x `mod` y == 0]

primeFactors :: Integer -> [Integer]
primeFactors x = (takeWhile (<=x) (facts x [])) where
    facts 1 [] = []
    facts 1 (y:ys) = y:ys
    facts m lst = facts (m `div` (leastFactor m)) ((leastFactor m):lst)

lcm :: [Integer] -> Integer
lcm [] = error "empty list"
lcm (x:xs) = product (commonFactors (x:xs) []) where
    commonFactors [] lst = lst
    commonFactors (x:xs) factors = commonFactors xs (union (sort (primeFactors x)) factors)

main = print (Main.lcm [1..20])
