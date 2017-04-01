; Problem 1

; I had to get massive amounts of help on these two problems and
; I assume that I will also have to on the next few

; cases
; exp is a symbol: variable use- return a list containing the exp
; car exp is lambda: abstraction - check to see if inner lcexp is a symbol and if so
	; check to see if it is inside the declaration of the outer expression
	; Then check to see if the inner lcexp is an abstraction as well
; else: application - if the first and second value are equal and they are both symbols, 
; return the value. If not, call free vars on the car and cadr
(define free-vars
	(lambda (exp)
		(cond 
			[(symbol? exp) (list exp)]
			[(equal? (car exp) 'lambda)
				(cond 
					[(symbol? (caddr exp))
						(if (member (caddr exp) (cadr exp))
							'() ; This means that it is bound
							(list (caddr exp)) ; This means that it is not bound
							)]
					[(equal? (caaddr exp) 'lambda) 
						(free-vars (caddr exp))]
					[else 
						(free-vars-helper (caddr exp) (cadr exp))])]
			[else
				(if (and (equal? (car exp) (cadr exp)) (symbol? (car exp)))
						(list (car exp))
						(append (free-vars (car exp)) (free-vars (cadr exp))))])))


(define free-vars-helper
	(lambda (x y)
		(if (null? x)
			'()
			(if (equal? y (car x)) 
				(free-vars-helper (car x) y)
				(append (list (car x)) (free-vars-helper (cdr x) y))))))

; Same frame work, handling the returns differently
(define bound-vars
	(lambda (exp)
		(cond
			[(symbol? exp) '()]
			[(equal? (car exp) 'lambda)
				(cond 
					[(symbol? (caddr exp))
						(if (member (caddr exp) (cadr exp))
							(list (caddr exp))
							'())]
					[(equal? (caaddr exp) 'lambda)
						(if (member (caddr (caddr exp)) (cadr exp))
							(list (caddr (caddr exp)))
							(bound-vars (caddr exp)))]
					[(equal? (caadr (caddr exp)) 'lambda) 
						(bound-vars (cadr (caddr exp)))]
					[else 
						(bound-vars-helper (caddr exp) (cadr exp))])]
			[else ; Minor differences on the application case -> No longer need to check if they are equals
				(if (symbol? (car exp))
					(bound-vars (cadr exp))
					(append (bound-vars (car exp)) (bound-vars (cadr exp))))])))

(define bound-vars-helper
	(lambda (x y)
		(if (null? x)
			'()
			(if (equal? y (car x))
				(append (list (car x)) (bound-vars-helper (cdr x) y))
				(bound-vars-helper (car x) y)))))



(define let-app-helper
	(lambda (ls)
		(cons 'lambda (cons (map car (cadr ls)) (cddr ls)))))

(define let->application
	(lambda (letexp)
		(cons (let-app-helper letexp) (map cadr (cadr letexp)))))

(define format-nested-lets
	(lambda (exp og)
		(if (null? (cdr exp))
			(list 'let (list (car exp)) (caddr og))
			(list 'let (list (car exp)) (format-nested-lets (cdr exp) og)))))

(define let*->let
	(lambda (exp)
		(format-nested-lets (cadr exp) exp)))


(define occurs-free?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda) 
       (and (not (member var (cadr exp)))
            (occurs-free? var (caddr exp))))
      [(equal? (car exp) 'set!) (equal? var (caddr exp))]
      [(equal? (car exp) 'let) (occurs-free? var (let->application exp))]
      [(equal? (car exp) 'let*) (occurs-free? var (let*->let exp))]
      [(equal? (car exp) 'if) (if (member var (cadr exp)) #t (occurs-free? var (caddr exp)))] 
      (else (or (occurs-free? var (car exp))
                (occurs-free? var (cdr exp)))))))

; (if:car (cond:cadr) (then:caddr) (else:cadddr))
; Friend gave me the idea of using our previous let-> procs

(define occurs-bound?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) #f]
      [(eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (member var (cadr exp))
                (occurs-free? var (caddr exp))))]
      [(equal? (car exp) 'set!) (if (or (equal? var (car exp)) (equal? var (cadr exp))) #f)]
      [(equal? (car exp) 'let) (occurs-bound? var (let->application exp))]
      [(equal? (car exp) 'let*) (occurs-bound? var (let*->let exp))]
      [(eqv? (car exp) 'if) (if (member var (cadr exp)) #f (occurs-free? var (caddr exp)))]
      [else (or (occurs-bound? var (car exp)) (occurs-bound? var (cdr exp)))])))
