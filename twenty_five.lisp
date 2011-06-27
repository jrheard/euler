(load "two.fas")

(defun first-fib-term-to-contain-n-digits (n)
  (labels ((f (term)
			 (if (>= (length (write-to-string (fib term))) n)
				 (progn (print (fib term)) term)
				 (f (1+ term)))))
		  (f 1)))

(time (print (first-fib-term-to-contain-n-digits 1000)))
