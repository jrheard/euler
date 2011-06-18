; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.
; Find the largest palindrome made from the product of two 3-digit numbers.

(defmethod palindromep ((lst list))
  (cond ((null lst) t)
		((eq (first lst) (car (last lst)))
		 (palindromep (cdr (butlast lst))))
		(t nil)))

(defmethod palindromep ((str string))
  (palindromep (coerce str 'list)))

(defmethod palindromep ((num number))
  (palindromep (write-to-string num)))
