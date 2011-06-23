(defun hotpo (n) ; half-or-triple-plus-one, according to wikipedia
  (if (evenp n)
	  (/ n 2)
	  (+ (* 3 n) 1)))

(defun hailstone-length (start seen)
  (labels ((f (num acc)
			 (cond ((eq 1 num) (1+ acc))
				   ((gethash num seen) (+ acc (gethash num seen)))
				   (t (f (hotpo num) (1+ acc))))))
		  (f start 0)))

(defun find-longest-hailstone (maximum)
  (let ((seen (make-hash-table)))
	   (labels ((f (i longest-start longest-chain)
				  (if (< i (/ maximum 2))
					  longest-start
					  (let ((len (hailstone-length i seen)))
						   (if (< len longest-chain)
							   (f (1- i) longest-start longest-chain)
							   (progn
								 (setf (gethash i seen) len)
								 (f (1- i) i len)))))))
			   (f maximum 0 0))))

(print (find-longest-hailstone 1000000))
