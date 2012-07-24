(print
  (let ((str (write-to-string (reduce #'+ (loop for i from 1 to 1000 collect (expt i i))))))
	   (subseq str (- (length str) 10))))
