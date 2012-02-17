-- Find the sum of the digits in the number 100!

import Data.Char

fact :: Integer -> Integer
fact x = product [1..x]

main = print $ sum $ map digitToInt $ show $ fact 100
