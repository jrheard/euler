; Returns the value of the nth diagonal number in a clockwise spiral starting from 1.
; (diagonal-number 0) = 1, (diagonal-number 1) = 3, (diagonal-number 5) = 13
(defun diagonal-number (n)
  (labels ((f (i acc)
			 (if (eq i 0)
				 (1+ acc)
				 (f (1- i) (+ acc (* 2 (ceiling (/ i 4))))))))
		  (f n 0)))

(time (print (loop for i from 0 to 2000 sum (diagonal-number i))))
