(load "/Users/jrheard/quicklisp/setup.lisp")
(ql:quickload "local-time")

(time (print
		(let ((count 0))
			 (do ((day (local-time:parse-timestring "1901-01-01T00:00:01-08:00") (local-time:timestamp+ day 1 :month)))
				 ((local-time:timestamp> day (local-time:parse-timestring "2000-12-31T00:00:01-08:00")))
				 (when (eq (local-time:timestamp-day-of-week day) 0)
					   (incf count)))
			 count)))
