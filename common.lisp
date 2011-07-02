(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(defun seq-from-file (filename delimiter)
  (reduce #'append
		  (with-open-file (infile filename :direction :input)
			(loop for line = (read-line infile nil)
			   while line collect (mapcar #'read-from-string (split-sequence:split-sequence delimiter line))))))
