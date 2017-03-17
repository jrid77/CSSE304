; Problem 1
(define curry2
	(lambda (func)
		(lambda (one)
			(lambda (two)
				(func one two)))))

; Problem 2
(define curried-compose
	(lambda (one)
		(lambda (two)
			(lambda (ls)
				(one (two ls))))))


; Problem 3
(define compose-helper
	(lambda (lofs obj)
		(if (null? (cdr lofs))
			((car lofs) obj)
			((car lofs) (compose-helper (cdr lofs) obj)))))

(define compose
	(lambda lofs
		(lambda (obj)
			(compose-helper lofs obj))))

; Problem 4
(define make-list-c
	(lambda (n)
		(lambda (obj)
			[cond 
			((zero? n) (list))
			((= 1 n) (list obj))
			(else (cons obj ((make-list-c (sub1 n)) obj)))])))

; Problem 5
; Needed quite a bit of clarification on this one

; Important to remember that (cadr ls) are the parameters and values for them
; Creates the lambda start
(define let-app-helper
	(lambda (ls)
		(cons 'lambda (cons (map car (cadr ls)) (cddr ls)))))

; Calls the helper and then tacks the parameters onto the end 
(define let->application
	(lambda (letexp)
		(cons (let-app-helper letexp) (map cadr (cadr letexp)))))

; Problem 6
(define format-nested-lets
	(lambda (exp og)
		(if (null? (cdr exp))
			(list 'let (list (car exp)) (caddr og))
			(list 'let (list (car exp)) (format-nested-lets (cdr exp) og)))))

(define let*->let
	(lambda (exp)
		(format-nested-lets (cadr exp) exp)))

; Problem 7
(define filter-in
	(lambda (pred? lst)
		(if (null? lst)
			'()
			(if (pred? (car lst))
				(cons (car lst) (filter-in pred? (cdr lst)))
				(filter-in pred? (cdr lst))))))
; Problem 8
(define filter-out
	(lambda (pred? lst)
		(if (null? lst)
			'()
			(if (not (pred? (car lst)))
				(cons (car lst) (filter-out pred? (cdr lst)))
				(filter-out pred? (cdr lst))))))

; Problem 9
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (list-sort string<? (map symbol->string los)))))

; Problem 10
(define invert
	(lambda (lop)
		(map cons (map cadr lop) (map list (map car lop)))))

; Problem 11
(define vector-index
	(lambda (pred? vector)
		(letrec 
			((vector-helper 
				(lambda (pred? lst n)
					(if (null? lst)
						#f
						(if (pred? (car lst))
							n
							(vector-helper pred? (cdr lst) (+ 1 n)))))))
			(vector-helper pred? (vector->list vector) 0))))

; Problem 12
(define ribassoc-helper
	(lambda (elem lst n)
		(if (null? lst)
			#f
			(if (equal? elem (car lst))
				n
				(ribassoc-helper elem (cdr lst) (+ 1 n))))))

(define ribassoc
	(lambda (s los v fail-value)
		(if (not (ribassoc-helper s los 0))
			fail-value
			(list-ref (vector->list v) (ribassoc-helper s los 0)))))