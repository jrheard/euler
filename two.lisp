; Each new term in the Fibonacci sequence is generated by adding the previous two terms.
; By starting with 1 and 2, the first 10 terms will be:
; 	1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;
; By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

(defun fib (index)
  (labels ((f (idx t1 t2)
			 (if (eq idx index)
				 (+ t1 t2)
				 (f (1+ idx) t2 (+ t1 t2)))))
		  (f 1 0 1)))

; tamir's version - way more elegant, but doesn't seem to optimize out
(defun tfib (index)
  (labels ((f (idx acc)
			 (cond ((eq idx 0) acc)
				   ((eq idx 1) (1+ acc))
				   (t (f (- idx 1) (f (- idx 2) acc))))))
		  (f index 0)))

(princ
  (loop for i from 1
	   for term = (fib i)
	   until (> term 4000000)
	   when (evenp term)
	   sum term))
