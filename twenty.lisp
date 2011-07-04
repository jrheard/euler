(load "common.lisp")

(defun altfact (n)
  (reduce #'* (loop for i from n downto 1 collect i)))

; turns out altfact performs *slightly* worse than fact. fun experiment, anyway.

(print (reduce-on-digits #'+ (write-to-string (fact 100))))
