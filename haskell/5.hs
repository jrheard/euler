-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

main = print (head [x | x <- [1..],
    foldl (\acc y -> acc && (x `mod` y == 0)) True [1..20]])
