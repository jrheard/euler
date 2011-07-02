(defun digit-to-word (digit place rest-of-num)
  (case place
		(1 (case (read-from-string digit)
				 (1 "one")
				 (2 "two")
				 (3 "three")
				 (4 "four")
				 (5 "five")
				 (6 "six")
				 (7 "seven")
				 (8 "eight")
				 (9 "nine")))
		(2 (if (and (eq 0 (read-from-string digit))
					(not (eq 0 (read-from-string (subseq rest-of-num (1- (length rest-of-num)))))))
			   "and "
			   (if (eq 1 (read-from-string digit))
				   (case (read-from-string (subseq rest-of-num 0 1))
						 (0 "ten")
						 (1 "eleven")
						 (2 "twelve")
						 (3 "thirteen")
						 (4 "fourteen")
						 (5 "fifteen")
						 (6 "sixteen")
						 (7 "seventeen")
						 (8 "eighteen")
						 (9 "nineteen"))
				   (let ((str (case (read-from-string digit)
									(2 "twenty")
									(3 "thirty")
									(4 "forty")
									(5 "fifty")
									(6 "sixty")
									(7 "seventy")
									(8 "eighty")
									(9 "ninety"))))
						(if (eq #\0 (car (coerce rest-of-num 'list)))
							str
							(format nil "~a-" str))))))
		(3 (if (eq 0 (read-from-string digit))
			   ""
			   (let ((str (format nil "~a hundred" (digit-to-word digit 1 nil))))
					(if (eq #\0 (car (coerce rest-of-num 'list)))
						str
						(format nil "~a and" str)))))
		(4 "one thousand")))

(defun num-to-words (num)
  (labels ((f (str acc)
			 (let ((digit (digit-to-word (subseq str 0 1) (length str) (subseq str 1))))
				  (if (and
						(> (length str) 1)
						(not (and
							   (equal "1" (subseq str 0 1))
							   (eq 2 (length str)))))
					  (f (subseq str 1) (concatenate 'string acc " " digit))
					  (concatenate 'string acc digit)))))
		  (string-trim " " (f (write-to-string num) ""))))

(print (length (remove #\Space (remove #\-
									   (funcall (lambda (lst) (apply #'concatenate (cons 'string lst)))
												(loop for i from 1 to 1000 collect (num-to-words i)))))))
