
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

(define first
	(lambda (slist stack)
		(stack 'push slist)))

(define advance
	(lambda (stack)		
		(if (stack 'empty?)
			#f
			(let ( [current (stack 'pop)] )
			(if (null? (car current))
				(if (null? (cdr current))
					(advance stack)
					(begin (stack 'push (cdr current)) (advance stack)))
				(if (list? (car current))
					(if (null? (cdr current))
						(begin (stack 'push (car current)) (advance stack)) 
						(begin (stack 'push (cdr current)) (stack 'push (car current)) (advance stack)))						
					(if (null? (cdr current))
						(car current)
						(begin (stack 'push (cdr current)) (car current)))))))))

(define make-slist-leaf-iterator
	(lambda (slist)
		(let ( [stack (make-stack)])
				(begin (stack 'push slist)
				(lambda (msg)
					(case msg
						[(next) (advance stack)]))))))

; Problem 4

; Check to see if the list is null and return empty list if so. This protects 
; against two things. An empty list input and against if the list size is a multiple
; of the n that is input

; Next check to see if the rest of the list is less than n. If so, put it into a list 
; Return it. This will allows the other lists to be consed onto that outer list

; Now if it is not either of those cases, then we need to make a list of them that is 
; that size and cons it onto the recursive call to the rest of the list

(define sublist
	(lambda (ls n i)
		(if (< i n)
			(sublist (cdr ls) n (add1 i))
			ls)))


(define get-n-list
	(lambda (ls n)
		(if (= 0 n)
			'()
			(cons (car ls) (get-n-list (cdr ls) (sub1 n))))))

(define group-by-n
	(lambda (ls n)
		(cond 
			[(null? ls) '()] 
			[(< (length ls) n) (list ls)]
			[else (cons (get-n-list ls n) (group-by-n (sublist ls n 0) n))])))

; Problem 5
; Needed quite a bit of help here. 
; I went and got help from a friend of mine that has already done the problem
; This was also my first time using a named let which turned out to be very useful
; I wish I would have started using them sooner. 
(define subst-leftmost
	(lambda (new old slist check?)
		(if (null? slist)
			'()
			(let loop ([cur (car slist)] [rest (cdr slist)]) 
				(cond 
					[(and (null? rest) (not (list? cur))) ; If it was a list and the rest was null, for instance, the ((b b)) case, it would not act how it was expected to.
						(if (check? old cur)
							(cons new '())
							(cons old '()))]
					[(or (symbol? cur) (null? cur))	; This was an idea i got from a friend			
						(if (check? old cur)
							(cons new rest)
							(cons cur (loop (car rest) (cdr rest))))]
					[(list? cur)
						(cons (loop (car cur) (cdr cur)) rest)])))))
