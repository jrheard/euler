(load "seven.fas")

(print (reduce #'+ (all-primes-less-than 2000000)))
