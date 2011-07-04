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

(time (print (nth 999999 (lexicographic-permutations '(0 1 2 3 4 5 6 7 8 9)))))
