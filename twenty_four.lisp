(load "common.lisp")

; first pass: calculates all permutations of digits in order. this is a little too slow for our purposes, though.
(defun lexicographic-permutations (digits)
  (let ((permutations '()))
	   (labels ((f (digits acc)
				  (if (null digits)
					  (push (format nil "~{~a~}" (coerce acc 'list)) permutations)
					  (loop for i from 0 to (1- (length acc))
						 for arr = (copy-seq acc)
						 when (null (aref arr i))
						 do (progn
							  (setf (aref arr i) (car digits))
							  (f (cdr digits) arr))))))
			   (f digits (make-array (length digits))))
	   (sort permutations #'string-lessp)))

;(time (print (nth 999999 (lexicographic-permutations '(0 1 2 3 4 5 6 7 8 9)))))

(defun nth-lexicographical-permutation (n digits)
  (labels ((index-to-insert-at (i arr)
			 (fresh-line)
			 (format t "~a ~a" i arr)
			 (let ((empty-slots (indices-matching #'null (coerce arr 'list))))
				  (if (not (cdr empty-slots))
					  (values (car empty-slots) 0)
					  (do* ((idx 0 (1+ idx))
							(num-permutations 0 (* idx (fact (1- (length empty-slots))))))
						   ((> (+ i num-permutations) n)
							(values (nth (1- idx) empty-slots) (- num-permutations (fact (1- (length empty-slots))))))
						   (fresh-line)
						   (format t "idx ~a num-permutations ~a" idx num-permutations)))))
		   (f (i digits arr)
			 (fresh-line)
			 (format t "~a ~a ~a" i digits arr)
			 (if (null digits)
				 arr
				 (multiple-value-bind (slot num-permutations) (index-to-insert-at i arr)
					  (fresh-line)
					  (format t "slot is ~a" slot)
					  (setf (aref arr slot) (car digits))
					  (f (+ i num-permutations) (cdr digits) arr)))))
		  (f 0 digits (make-array (length digits)))))

;....shit. this function gives you the nth permutation, but not the nth *sorted* permutation. fuck.
