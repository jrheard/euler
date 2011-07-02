(defun divisors (num)
  (remove-if (lambda (n) (eq num n))
			 (apply #'append
					(loop for i from 1 to (sqrt num)
					   when (eq (mod num i) 0)
					   collect (list i (/ num i))))))

(time
  (let ((amicables '()))
	   (loop for i from 1 to 10000
		  for suma = (reduce #'+ (divisors i))
		  for sumb = (reduce #'+ (divisors suma))
		  when (and
				 (eq sumb i)
				 (not (eq suma i))
				 (not (member i amicables)))
		  do (progn
			   (push i amicables)
			   (push suma amicables)))
	   (print (reduce #'+ amicables))))
