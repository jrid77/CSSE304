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

; Define Syntax Examples

; (define-syntax <id>
; 	(syntax-rules ( {<keyword>}* )
; 		{ [pattern template] }* ))

; Pattern is what is getting expanded and template is what it gets expanded into

(define my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
		((lambda (x ...) e1 e2 ...)
			v ...)]))
; _ is assumbed to be the name of the form
; e2 ... means that there can be 0 or more values of e2 used
; x ... means all of the first elements as a part of that declaration

(define-syntax my-if
	(syntax-rules (then else)
		[(_ e1 then e2)
		(if e1 e2)]
		[(_ e1 then e2 else e3)
		(if e1 e2 e3)]
		))

(define-syntax ++
	(syntax-rules ()
		[(_ e1)
		(begin (set! e1 (add1 e1)) e1)]))

(define-syntax +++
	(syntax-rules ()
		[(_ e1)
		(let ([og e1])
			(++ e1)
			og)]))

(define-syntax my-and
	(syntax-rules ()
		[(_) #t]
		[(_ e) e]
		[(_ e1 e2 ...) 
		(if e1
			(my-and e2 ...)
			#f)]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...)
			body ...)
		(begin
			init ...
			(let for-loop ()
				(if test
					(begin 
						body ...
						update ...
						(for-loop))
					)))]))


; Abstract Datatypes

(define zero list)

(define iszero? null?)

(define pred cdr)

(define succ (lambda (n) (cons #t n)))

