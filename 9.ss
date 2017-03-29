; Problem 1
; sn-list-recur

(define snlist-recur
	(lambda (base proc-list proc-num)
		(letrec
			([helper 
				(lambda (snlist)
					(cond 
						[(null? snlist) base]
						[(list? (car snlist)) (proc-list (helper (car snlist)) (helper (cdr snlist)))]
						[else (proc-num (car snlist) (helper (cdr snlist)))]))
					])
			helper)))

(define sn-list-sum
	(snlist-recur 0 + +))

(define sn-list-map
	(lambda (proc slist)
		((snlist-recur '() cons (lambda (x y) (cons (proc x) y))) slist)))

(define sn-list-paren-count
	(snlist-recur 2 + (lambda (x y) y)))

(define sn-list-reverse
	(snlist-recur '() (lambda (x y) (append y (list x))) (lambda (x y) (append y (list x)))))

(define sn-list-occur
	(lambda (elem slist)
		((snlist-recur 0 + (lambda (x y) 
			(if (equal? x elem)
				(+ 1 y)
				y))) 
		slist)))

(define sn-list-depth
	(snlist-recur 
		1
		(lambda (x y) 
			(if (>= x y)
				(+ 1 x)
				y)) 
		(lambda (x y) y)))


; Problem 2
; The cond used here was originally due to me thinking that the base case
; was a null check, however, when we have numbers as leaves, we can use them as the base case
(define bt-recur
	(lambda (num-proc sym-proc)
		(letrec
			([helper
				(lambda (bt)
					(cond
						[(number? bt) (num-proc bt)]
						[else (sym-proc (helper (cadr bt)) bt (helper (caddr bt)))]))])
			helper)))

(define bt-sum
	(bt-recur + (lambda (x y z) (+ x z))))

(define bt-inorder
	(bt-recur (lambda (x) '()) (lambda (x y z) (append x (list (car y)) z))))


; Problem 3
; I originally did this problem with a recursive helper but I read
; that you said to do it with multiple maps so I adjusted my code 
; accordingly
(define list-of-functions
	(lambda (loc)
		(map (lambda (x) 
			(if (equal? #\a x)
				'car
				'cdr))
		loc)))

(define make-c...r
	(lambda (str)
		(apply compose (map eval (list-of-functions (string->list str))))))


(define compose
 (case-lambda
 	[() (lambda (x) x)]
	[(first . rest)
 		(let ([composed-rest (apply compose rest)])
 			(lambda (x) (first (composed-rest x))))]))