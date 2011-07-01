(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "split-sequence")

(defparameter *tree*
	(let ((seq
			(reduce #'append
					(with-open-file (infile "eighteen_tree.txt" :direction :input)
					  (loop for line = (read-line infile nil)
						   while line collect (mapcar #'read-from-string (split-sequence:split-sequence #\Space line)))))))
		 (make-array (length seq) :initial-contents seq)))

(defun level (node)
  ; there must be a more efficient way to do this
  (labels ((f (i acc)
			 (if (<= (1+ node) acc)
				 (- i 2) ; "level in the tree" is indexed from 0
				 (f (1+ i) (+ acc i)))))
		  (f 1 0)))

(defun left (tree node)
  (+ (1+ node) (level node)))

(defun right (tree node)
  (+ (1+ node) (level node) 1))

(defun paths (tree)
  (let ((paths '()))
	   (labels ((f (node acc)
				  (if (and (< (left tree node) (length tree)) (< (right tree node) (length tree)))
					  (progn
						(f (left tree node) (cons (aref tree (left tree node)) acc))
						(f (right tree node) (cons (aref tree (right tree node)) acc)))
					  (push (reverse acc) paths))))
			   (f 0 (list (aref tree 0))))
	   paths))

(compile 'paths)

(time (print (reduce #'max (mapcar (lambda (lst) (reduce #'+ lst)) (paths *tree*)))))

; TODO for the harder version of this problem (#67), thoughts so far are to start at the bottom of the graph and try to somehow group/bubble up sums from there
