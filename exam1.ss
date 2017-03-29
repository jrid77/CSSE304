; Problem 1
(define max-diff
	(lambda (lon)
		(- (apply max lon) (apply min lon))))

; Problem 2
(define get-n-times
	(lambda (obj ls)
		(if (member obj ls)
			(+ 1 (get-n-times obj (cdr (member obj ls))))
			0)))

(define member-n-times?
	(lambda (obj ls n)
		(<= n (get-n-times obj ls))))

; Problem 3
(define count-n
	(lambda (obj lon)
		(if (null? lon)
			0
			(if (equal? obj (car lon))
				(+ 1 (count-n obj (cdr lon)))
				(count-n obj (cdr lon))))))

(define iter-lon
	(lambda (lon rest accum)
		(if (null? rest)
			(car accum)
			(if (not (car accum))
				(iter-lon 
					lon
					(cdr rest)
					(list (car rest) (count-n (car rest) lon)))
				(if (and (>= (count-n (car rest) lon) (cadr accum)) (< (car rest) (car accum)))
					(iter-lon 
						lon 
						(cdr rest) 
						(list (car rest) (count-n (car rest) lon)))
					(iter-lon
						lon
						(cdr rest)
						accum))))))

(define most-frequent
	(lambda (lon)
		(iter-lon lon lon '(#f 0))))

; Problem 4
(define slist-same?
	(lambda (s1 s2 sym-comparator)
		(if (null? s1)
			(null? s2)
			(if (list? s1)
				(and 
					(list? s2)
					(slist-same? (car s1) (car s2) sym-comparator) 
					(slist-same? (cdr s1) (cdr s2) sym-comparator))
				(sym-comparator s1 s2)))))


; Problem 5
; I copied make-stack in for an example from one of the inclass code-alongs
; It is only here for me to look at and it is not used. 
(define make-stack
	(lambda ()
		(let ([stk '()])
				(lambda (msg . args)			
					(case msg
						[(empty?) (null? stk)]
						[(push) (set! stk (cons (car args) stk))]
						[(pop) (let ([top (car stk)])
							(set! stk (cdr stk))
							top)]
						[else (error 'stack "illegeal stack message: " msg)])))))

(define make-lyst
	(lambda ()
		(let* (
			[lyst '()]
			[first '()]
			[last '()])
		(lambda (msg . args)
			(case msg
				[(len) (length lyst)]
				[(add-first) 
				(begin
					(set! lyst (cons (car args) lyst))
					(set! first (car lyst)))]
				[(add-last)
				(begin
					(set! lyst (append lyst (list (car args))))
					(set! last (car args)))]
				[(first)
				first]
				[(last)
				last]
				[(get)
				(list-ref lyst (car args))])))))