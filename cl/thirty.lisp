(defun can-be-written-as-sum-of-nth-powers-of-its-digits (num n)
  (let ((str (write-to-string num)))
	   (eq num
		 (loop for i from 0 to (1- (length str))
			  sum (expt (parse-integer str :start i :end (1+ i)) n)))))

(time (print (loop for i from 2 to 200000 when (can-be-written-as-sum-of-nth-powers-of-its-digits i 5) sum i)))
