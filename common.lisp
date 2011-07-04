(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(defun seq-from-file (filename delimiter)
  (reduce #'append
		  (with-open-file (infile filename :direction :input)
			(loop for line = (read-line infile nil)
			   while line collect (mapcar #'read-from-string (split-sequence:split-sequence delimiter line))))))

(defun fact (n)
  (labels ((f (num acc)
			 (if (eq num 1)
				 acc
				 (f (1- num) (* acc num)))))
		  (f n 1)))

(defun reduce-on-digits (fun str)
  (reduce fun (loop for i from 1 until (> i (length str)) collect (parse-integer str :start (- i 1) :end i))))

(defun indices-matching (fun lst)
  (let ((indices '()))
	   (loop for i from 0 to (1- (length lst))
		  when (funcall fun (nth i lst))
		  do (push i indices))
	   (nreverse indices)))
