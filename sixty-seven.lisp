(load "eighteen.lisp")

(defparameter *tree* (read-tree "triangle.txt"))
(time (print (max-path-sum *tree*)))
