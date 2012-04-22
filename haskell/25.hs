-- The 12th term of the Fibonacci series is 144. It's the first term to contain 3 digits.
-- What is the first term in the Fibonacci sequence to contain 1000 digits?

import Common
import Data.List

firstFibTermWithNumDigits :: Int -> Int
firstFibTermWithNumDigits numDigits = case maybeTerm of
    Just term -> term + 1
    Nothing -> -1
    where
        maybeTerm = findIndex (\x -> length (show x) >= 1000) fibonacci

main = print $ firstFibTermWithNumDigits 1000
