(let ((seen (make-hash-table)))
	 (defun divisors (num)
	   (labels ((f (divisor divs)
				  (if (<= divisor 1)
					  (append divs '(1))
					  (if (eq (mod num divisor) 0)
						  (let ((val (gethash divisor seen)))
								(if val
									(append val divs)
									(f (1- divisor) (cons divisor divs))))
						  (f (1- divisor) divs)))))
			   (setf (gethash num seen) (f (1- num) (list num))))))

(defun first-triangle-number-with-over-num-divisors (num)
  (labels ((f (triangle i)
			 (if (> (length (divisors triangle)) num)
				 triangle
				 (f (+ triangle i) (1+ i)))))
		  (f 1 2)))

(print (first-triangle-number-with-over-num-divisors 10))
