(defun hotpo (n) ; half-or-triple-plus-one, according to wikipedia
  (if (evenp n)
	  (/ n 2)
	  (+ (* 3 n) 1)))

(defun hailstone (start)
  (labels ((f (num acc)
			 (if (eq 1 num)
				 (cons num acc)
				 (f (hotpo num) (cons num acc)))))
		  (reverse (f start '()))))

(defun find-longest-hailstone (maximum)
  (let ((seen (make-hash-table)))
	   (labels ((f (i longest-start longest-chain)
				  (if (eq i 1)
					  longest-start
					  (if (gethash i seen)
						  (f (1- i) longest-start longest-chain)
						  (let ((seq (hailstone i)))
							   (if (> (length seq) longest-chain)
									 (progn
									   (mapcan (lambda (term) (setf (gethash term seen) t)) seq)
									   (f (1- i) i (length seq)))
								   (f (1- i) longest-start longest-chain)))))))
			   (f maximum 0 0))))

(print (find-longest-hailstone 1000000))
