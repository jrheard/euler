; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
; a^2 + b^2 = c^2
; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;
; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
; Find the product abc.

(labels ((funa (b c)
		   (- 1000 b c))
	  (func (b)
		(/ (+ (- 500000 (* 1000 b)) (* b b)) (- 1000 b))))
	 (loop for b from 999 downto 1
		  for c = (func b)
		  for a = (funa b c)
		  when (and
				 (< 0 a b c)
				 (eq (+ (* a a) (* b b)) (* c c))
				 (eq (+ a b c) 1000))
		  do (print (* a b c))))
