; This works tail recursively so that it does not create more than one stack 
; frome at a time
(define fact-tail
	(lambda (n accum)
		(if (zero? n)
			accum
			(fact-tail (- n 1) (* n accum)))))

; Renaming can break things very easily
; This is fixed by using inner helper functions
(define fact-tail
	(letrec ([fact-help (lambda (n accum)
		(if (zero? n)
			accum
			(fact-help (- n 1) (* n accum))))])
	(lambda (n) (fact-help n 1))))

; Creating OOP in Scheme through stacks

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


(define member?
	(lambda (item ls)
		((member-c? item) ls)))

(define member-c?
	(lambda (item)
		(letrec 
			([helper
				(lambda (ls)
					(if (null? ls)
						#f
						(or (equal? (car ls) item) (helper (cdr ls)))))])
			helper)))

(define map
	(lambda (proc ls)
		((apply-to-all proc) ls)))

(define apply-to-all
	(lambda (proc)
		(letrec
			([helper
				(lambda (ls)
					(if (null? ls)
						'()
						(cons (proc (car ls)) (helper (cdr ls)))))])
			helper)))


; This is a function that replaces the others
; It creates the above values
; It takes in two args: base-value and list-procedure
(define list-recur
	(lambda (base proc)
		(letrec
			([helper 
				(lambda (ls)
					(if (null? ls)
						base
						(proc (car ls) (helper (cdr ls)))))])
			helper)))

(define list-sum
	(list-recur 0 +))

(define list-prod
	(list-recur 1 *))

(define apply-to-all
	(lambda (proc)
		(list-recur '() (lambda (x y) (cons (proc x) y)))))

(define member-c?
	(lambda (item)
		(list-recur #f (lambda (x y) (or (equal? x item) y)))))

(define length
	(list-recur 0 (lambda (x y) (add1 y))))

(define filter
	(lambda (proc)
		(list-recur '() (lambda (x y) (if (proc x) 
										  (cons x y)
										  y)))))
