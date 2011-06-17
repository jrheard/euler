; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defun divisible-by-1-to-20p (num)
  (let ((divisible t))
	   (loop for i from 1 to 20
			when (not (zerop (mod num i)))
			do (setf divisible nil))
	   divisible))

(defun factors (num)
  (labels ((find-factor (n cur)
					   (if (zerop (mod n cur))
						 cur
						 (find-factor n (1+ cur))))
		   (f (n factors)
			 (if (eq n 1)
				 factors
				 (let ((factor (find-factor n 2)))
					  (f (/ n factor) (cons factor factors))))))
		   (f num '())))

; TODO it'd be fun to write a macro approximating defaultdict behavior

(defun occurrences-list-to-counts-hash (occurrences)
  (labels ((f (lst hash)
			 (if lst
				 (progn
				   (if (gethash (car lst) hash)
					   (incf (gethash (car lst) hash))
					   (setf (gethash (car lst) hash) 1))
				   (f (cdr lst) hash)))
				 hash))
		  (f occurrences (make-hash-table))))

(defparameter *factors* (make-hash-table))

(loop for i from 1 to 20
	 for cur-facts = (occurrences-list-to-counts-hash (factors i))
	 do (loop for key being the hash-keys of cur-facts
			 do (progn
				  (if (gethash key *factors*)
					  (setf (gethash key *factors*) (max (gethash key *factors*) (gethash key cur-facts)))
					  (setf (gethash key *factors*) (gethash key cur-facts))))))

(defparameter *lcm* 1)

(loop for key being the hash-keys of *factors*
	 using (hash-value value)
	 do (loop for i from 1 to value
			 do (setf *lcm* (* *lcm* key))))

(print *lcm*)
