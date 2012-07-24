(load "common.lisp")

(defun name-score (name)
  (let ((acc 0))
	   (dolist (c (coerce name 'list))
			   (incf acc (- (char-code c) 64)))
	   acc))

(defparameter names (sort (seq-from-file "names.txt" #\,) #'string-lessp))

(time (print
		(let ((len (length names)))
			 (labels ((f (lst idx acc)
						(if (> idx len)
							acc
							(f (cdr lst) (1+ idx) (+ acc (* (name-score (car lst)) idx))))))
					 (f names 1 0)))))
