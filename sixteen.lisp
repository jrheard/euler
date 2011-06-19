(load "eight.lisp")

(print (reduce-on-digits #'+ (write-to-string (expt 2 1000))))
