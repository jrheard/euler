(defun divisors (num)
  (labels ((f (i divs)
		   (if (> i (/ num 2))
			   divs
			   (if (eq (mod num i) 0)
				   (f (1+ i) (append (list i (/ num i)) divs))
				   (f (1+ i) divs)))))
	   (remove-duplicates (f 1 '()))))

(defun first-triangle-number-with-over-num-divisors (num)
  (labels ((f (triangle i)
			 (if (> (length (divisors triangle)) num)
				 triangle
				 (f (+ triangle i) (1+ i)))))
		  (f 1 2)))

(time (first-triangle-number-with-over-num-divisors 60))


; time for first-triangle of 60 in current version
; Real time: 0.501909 sec.
; Run time: 0.463359 sec.
; Space: 7619292 Bytes
; GC: 14, GC time: 0.044082 sec.




;time for first-triangle of 60 in old version
; Real time: 15.30771 sec.
; Run time: 13.309559 sec.
; Space: 255540 Bytes
