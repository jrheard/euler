; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
; What is the 10001st prime number?

(defun all-primes-less-than (num)
  (let ((candidates (make-hash-table)))
	   (loop for i from 1 to num do (setf (gethash i candidates) t))
	   (labels ((f (cur primes)
				  (if (< cur num)
					  (if (gethash cur candidates)
						  (progn
							(loop for i from 1
								 for multiple = (* i cur)
								 while (< multiple num)
								 do (setf (gethash multiple candidates) nil))
							(f (1+ cur) (cons cur primes)))
						  (f (1+ cur) primes))
					  primes)))
				(f 2 '()))))
