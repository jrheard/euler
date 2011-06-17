; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defun divisible-by-1-to-20p (num)
  (let ((divisible t))
	   (loop for i from 1 to 20
			when (not (zerop (mod num i)))
			do (setf divisible nil))
	   divisible))

(defun factors (num)
  (labels ((find-factor (n cur)
					   (if (zerop (mod n cur))
						 cur
						 (find-factor n (1+ cur))))
		   (f (n factors)
			 (if (eq n 1)
				 factors
				 (let ((factor (find-factor n 2)))
					  (f (/ n factor) (cons factor factors))))))
		   (f num '())))
