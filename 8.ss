
; Problem 1
; PREFACE:
; i am not doing a cond for these because when working with slists 
; for the first time, i am finding it helps my understanding to
; bring the base case out of the other two. 
(define slist-map
	(lambda (proc slist)
		(if (null? slist)
			'()
			(if (list? slist) 
				(cons (slist-map proc (car slist)) (slist-map proc (cdr slist)))
				(proc slist)))))

(define slist-reverse
	(lambda (slist)
		(if (null? slist)
			'()
			(if (list? (car slist))
				(append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))
				(append (slist-reverse (cdr slist)) (list (car slist)))))))

; I wrote this as a reference for slist-reverse so that I had a base algorithm to work with
(define rev
	(lambda (ls)
		(if (null? ls)
			'()
			(append (rev (cdr ls)) (list (car ls))))))

(define slist-paren-count
	(lambda (slist)
		(if (null? slist)
			2
			(if (list? (car slist))
				(+ (slist-paren-count (car slist)) (slist-paren-count (cdr slist)))
				(+ (slist-paren-count (cdr slist)))))))

(define slist-depth-helper
	(lambda (slist accum)
		(if (null? slist)
			accum
			(if (list? (car slist))
				(max (slist-depth-helper (car slist) (+ 1 accum)) (slist-depth-helper (cdr slist) accum))
				(max accum (slist-depth-helper (cdr slist) accum))))))

(define slist-depth
	(lambda (slist)
		(slist-depth-helper slist 1)))

(define symbols-helper
	(lambda (slist d accum)
		(if (null? slist)
			'()
			(if (list? (car slist))
				(append (symbols-helper (car slist) d (+ 1 accum)) 
					(symbols-helper (cdr slist) d accum))
				(if (= d accum)
					(append (list (car slist)) (symbols-helper (cdr slist) d accum))
					(symbols-helper (cdr slist) d accum))))))

(define slist-symbols-at-depth
	(lambda (slist d)
		(symbols-helper slist d 1)))

; Problem 2
(define group-by-two
	(lambda (ls)
		(cond 
			[(null? ls) '()]
			[(null? (cdr ls)) (list (list (car ls)))]
			[else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])))

; Problem 3

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
