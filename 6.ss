(define curry2
	(lambda (func)
		(lambda (one)
			(lambda (two)
				(func one two)))))

(define curried-compose
	(lambda (one)
		(lambda (two)
			(lambda (ls)
				(one (two ls))))))

(define compose-helper
	(lambda (lofs obj)
		(if (null? (cdr lofs))
			((car lofs) obj)
			((car lofs) (compose-helper (cdr lofs) obj)))))

(define compose
	(lambda lofs
		(lambda (obj)
			(compose-helper lofs obj))))

(define make-list-c
	(lambda (n)
		(lambda (obj)
			[cond 
			((zero? n) (list))
			((= 1 n) (list obj))
			(else (cons obj ((make-list-c (sub1 n)) obj)))])))

