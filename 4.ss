; Problem 1
; Check to see if it is a list
; Check to see if it is a set
; Check to see if last element of each sublist is a positive integer
(define pos-int?
	(lambda (n)
		(if (integer? n)
			(positive? n)
			#f)))

(define l-of-firsts
	(lambda (l)
		(if (null? l)
			'()
			(cons (caar l) (l-of-firsts (cdr l))))))

(define check-repeats? 
	(lambda (l)
		(if (null? l)
			#t
			(if (member (car l) (cdr l))
				#f
				(check-repeats? (cdr l))))))

(define multi-helper?
	(lambda (lol)
		(if (null? lol)
			#t
			(if (and (not (pos-int? (caar lol))) (pos-int? (cadar lol)))
				(multi-helper? (cdr lol))
				#f))))

(define multi-set?
	(lambda (obj)
		(cond 
			[(not (relation? obj)) #f]
			[(not (check-repeats? (l-of-firsts obj))) #f]
			[else (multi-helper? obj)]
			)))


; Problem 2
(define ms-size
	(lambda (ms)
		(apply + (map cadr ms))))

; Problem 3
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))

; Problem 4
; Check to see if it is a list
; Check to see if inner elements are lists
; Check to see if all inner elements have the same length

(define inner-list-checker?
	(lambda (l)
		(if (null? l)
			#t
			(if (list? (car l))
				(inner-list-checker? (cdr l))
				#f))))

(define length-checker?
	(lambda (n l)
		(if (null? l)
			#t
			(if (and (not (= 0 n)) (= (length (car l)) n))
				(length-checker? n (cdr l))
				#f))))

(define matrix?
	(lambda (obj)
		(cond 
			[(not (list? obj)) #f]
			[(not (inner-list-checker? obj)) #f]
			[(not (length-checker? (length (car obj)) obj)) #f]
			[else #t])))

; Problem 5
; Got help with this problem. Final solution is very elegant in my opinion
(define matrix-transpose
	(lambda (m)
		(if (= 0 (length (car m)))
			'()
			(cons (map car m) (matrix-transpose (map cdr m))))))

; Problem 6
(define last
	(lambda (l)
		(if (null? (cdr l))
			(car l)
			(last (cdr l)))))

; Problem 7
(define all-but-last
	(lambda (l)
		(if (null? (cdr l))
			'()
			(cons (car l) (all-but-last (cdr l))))))


; Pulled from previous assignments
(define set-helper?
	(lambda (check l)
		(if (null? l)
			#t
			(if (equal? check (car l))
				#f
				(set-helper? check (cdr l))))))

(define set?
	(lambda (l)
		(if (and (list? l) (null? l))
			#t
			(and (set-helper? (car l) (cdr l)) (set? (cdr l))))))

(define all-lists?
	(lambda (obj)
		(if (null? obj)
			#t
			(if (list? (car obj))
				(all-lists? (cdr obj))
				#f))))

(define all2?
	(lambda (obj)
		(if (null? obj)
			#t
			(if (= 2 (length (car obj)))
				(all2? (cdr obj))
				#f))))

(define relation?
	(lambda (obj)
		(cond [(not (list? obj)) #f]
			  [(not (set? obj)) #f]
			  [(not (all-lists? obj)) #f]
			  [(not (all2? obj)) #f]
			  [else #t])))
