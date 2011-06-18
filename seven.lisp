; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
; What is the 10001st prime number?

(defun primep (num primes)
  (cond ((null primes) t)
		((eq (mod num (car primes)) 0) nil)
		(t (primep num (cdr primes)))))

(defparameter *primes* '())

; naive implementation; do that sieve thing later
(loop for i from 2
	 until (eq (length *primes*) 10001)
	 do (when (primep i *primes*)
			  (progn (print (length *primes*))
			(setf *primes* (cons i *primes*)))))
