-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

middle :: [a] -> [a]
middle = tail . init

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = if (head xs) == (last xs) then (isPalindrome (middle xs)) else False

numIsPalindrome :: Int -> Bool
numIsPalindrome = isPalindrome . show

main = print $ maximum $ filter numIsPalindrome [x * y | x <- [100..999], y <- [100..999]]
