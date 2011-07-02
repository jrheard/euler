(load "common.lisp")

(defun name-score (name)
  (labels ((f (str acc)
			 (if (equal str "")
				 acc
				 (f (subseq str 1) (+ acc (- (char-code (car (coerce str 'list))) 64))))))
		  (f name 0)))

(defparameter names (sort (seq-from-file "names.txt" #\,) #'string-lessp))

(time (print
		(let ((len (length names)))
			 (labels ((f (lst idx acc)
						(if (> idx len)
							acc
							(f (cdr lst) (1+ idx) (+ acc (* (name-score (car lst)) idx))))))
					 (f names 1 0)))))
