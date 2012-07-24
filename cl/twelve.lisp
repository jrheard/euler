(load "five.fas")

(defun num-divisors (num)
  (let* ((facts (factors num))
		(counts (occurrences-list-to-counts-hash facts)))
	   (reduce #'*
			   (loop for key being the hash-keys of counts
					collect (1+ (gethash key counts))))))

(defun first-triangle-number-with-over-num-divisors (num)
  (labels ((f (triangle i)
			 (if (> (num-divisors triangle) num)
				 triangle
				 (f (+ triangle i) (1+ i)))))
		  (f 1 2)))

(time (print (first-triangle-number-with-over-num-divisors 500)))
