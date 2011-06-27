(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(defparameter *tree*
	(let ((seq
			(reduce #'append
					(with-open-file (infile "eighteen_tree_small.txt" :direction :input)
					  (loop for line = (read-line infile nil)
						   while line collect (mapcar #'read-from-string (split-sequence:split-sequence #\Space line)))))))
		 (make-array (length seq) :initial-contents seq)))

(defun aref-safe (arr idx)
  (if (< idx (length arr))
	  (aref arr idx)
	  nil))

(defun level (node)
  (truncate (log (1+ node) 2)))

(defun left (node)
  (aref-safe *tree* (+ (1+ node) (level node))))

(defun right (node)
  (aref-safe *tree* (+ (1+ node) (level node) 1)))
