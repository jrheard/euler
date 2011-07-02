(load "common.lisp")

(defun read-tree (filename)
  (let ((seq (seq-from-file filename #\space)))
	   (make-array (length seq) :initial-contents seq)))

(defparameter *tree* (read-tree "eighteen_tree.txt"))

(defun level (node)
										; there must be a more efficient way to do this
  (labels ((f (i acc)
			 (if (<= (1+ node) acc)
				 (- i 2) ; "level in the tree" is indexed from 0
				 (f (1+ i) (+ acc i)))))
		  (f 1 0)))

(defun level-start (level)
  (reduce #'+ (loop for i from 1 to level collect i)))

(defun level-end (level)
  (1- (level-start (1+ level))))

(defun right-parent (node)
  (let ((lev (level node)))
	   (if (eq node (level-end lev))
		   nil
		   (- node (- (level-end lev) (level-start lev))))))

(defun left-parent (ndoe)
  (let ((lev (level node)))
	   (if (eq node (level-start lev))
		   nil
		   (- node (- (level-end lev) (level-start lev) 1)))))

(defun max-path-sum (tree) ; destructive to tree :/
  (let ((tree (copy-seq tree)))
	   (labels ((f (level)
				  (if (eq level 0)
					  (aref tree 0)
					  (progn
						(loop for i
							 from (level-start level)
							 to (1- (level-end level))
						   do (let ((parent (right-parent i)))
								   (setf (aref tree parent)
										 (+ (aref tree parent) (max (aref tree i) (aref tree (1+ i)))))))
						(f (1- level))))))
			   (f (level (1- (length tree)))))))

(time (print (max-path-sum *tree*)))

(print *tree*)
