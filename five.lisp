; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defun divisible-by-1-to-20p (num)
  (let ((divisible t))
	   (loop for i from 1 to 20
			when (not (zerop (mod num i)))
			do (setf divisible nil))
	   divisible))

(defparameter *divisors* (loop for i from 1 to 20 collect i))

(loop
	 while (not (eq (length (remove-duplicates *divisors*)) 1))
	 do (let* ((smallest (apply #'min *divisors*))
			   (index (position smallest *divisors*)))
			  (setf (nth index *divisors*)
						 (+ smallest (1+ index)))))

(print (first *divisors*))
(print (divisible-by-1-to-20p (first *divisors*)))
