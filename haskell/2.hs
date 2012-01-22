-- Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fib :: (Ord a, Num a) => a -> a
fib x
    | x <= 1 = 1
    | x == 2 = 2
    | otherwise = fib (x - 1) + fib (x - 2)

main = print (sum [x | x <- takeWhile (< 4000000) [x | x <- [1..]], even x])
