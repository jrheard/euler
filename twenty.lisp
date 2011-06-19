(load "eight.lisp")

(defun fact (n)
  (labels ((f (num acc)
			 (if (eq num 1)
				 acc
				 (f (1- num) (* acc num)))))
		  (f n 1)))

(defun altfact (n)
  (reduce #'* (loop for i from n downto 1 collect i)))

; turns out altfact performs *slightly* worse than fact. fun experiment, anyway.

(print (reduce-on-digits #'+ (write-to-string (fact 100))))
