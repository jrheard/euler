(defun routes-through-a-grid (x y)
  (let ((choices (make-hash-table :test #'equal)))
	   (setf (gethash '(1 . 1) choices) 1)
	   (loop for yy from 1 to y
			do (loop for xx from 1 to x
					when (not (and (eq xx 1) (eq yy 1)))
					do (setf (gethash (cons xx yy) choices)
							 (+ (or (gethash (cons (1- xx) yy) choices) 0)
								(or (gethash (cons xx (1- yy)) choices) 0)))))
	   (incf (gethash '(1 . 1) choices)) ; (1, 1) always has value 2, but calculations are performed initially as if it had value 1
	   (reduce #'+ (loop for val being the hash-values of choices collect val))))

(time (print (routes-through-a-grid 20 20)))
