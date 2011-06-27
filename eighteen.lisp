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

(defun max-tree-path (tree)
  (let ((maximum 0))
	   (labels ((f (node acc)
				  (when (> acc maximum) (setf maximum acc))
				  (when (< node (length tree))
						(f (left tree node) (+ acc (aref-safe tree (left tree node))))
						(f (right tree node) (+ acc (aref-safe tree (right tree node)))))))
			   (f 0 0))
	   maximum))

(compile 'max-tree-path)

(time (print (max-tree-path *tree*)))
