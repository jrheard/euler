; The prime factors of 13195 are 5, 7, 13 and 29.
; What is the largest prime factor of the number 600851475143 ?

; factors definition copy-pasted from five.lisp, where i first wrote/used it
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
