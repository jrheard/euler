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
