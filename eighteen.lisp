(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(defparameter *tree*
	(let ((seq
			(reduce #'append
					(with-open-file (infile "eighteen_tree.txt" :direction :input)
					  (loop for line = (read-line infile nil)
						   while line collect (mapcar #'read-from-string (split-sequence:split-sequence #\Space line)))))))
		 (make-array (length seq) :initial-contents seq)))

(defun aref-safe (arr idx)
  (if (< idx (length arr))
	  (aref arr idx)
	  0))

(defun level (node)
  (truncate (log (1+ node) 2)))

(defun left (tree node)
  (+ (1+ node) (level node)))

(defun right (tree node)
  (+ (1+ node) (level node) 1))

(defun paths (tree)
  (let ((paths '()))
	   (labels ((f (node acc)
				  (if (and (<= (left tree node) (length tree)) (<= (right tree node) (length tree)))
					  (progn
						(f (left tree node) (cons (aref-safe tree (left tree node)) acc))
						(f (right tree node) (cons (aref-safe tree (right tree node)) acc)))
					  (push acc paths))))
			   (f 0 (list (aref tree 0))))
	   paths))

(compile 'paths)

(time (print (reduce #'max (mapcar (lambda (lst) (reduce #'+ lst)) (paths *tree*)))))
