; Problem 1
; NEED HELP HERE
(define sort-lists
	(lambda (lst)
		(list-sort (lambda (x y) (< (car x) (car y))) lst)))

(define interval-intersects?
	(lambda (i1 i2)
		(if (<= (car i1) (cadr i2))
			(<= (car i2) (cadr i1))
			(>= (car i2) (cadr i1)))))

(define interval-union 
	(lambda (i1 i2)
		(if (interval-intersects? i1 (car i2))
			(cons (list (min (car i1) (caar i2)) (max (cadr i1) (cadar i2))) (cdr i2))
			(cons i1 i2))))

(define minimize-interval-list-helper
	(lambda (lst)
		(if (null? (cdr lst))
			lst
			(interval-union (car lst) (minimize-interval-list-helper (cdr lst))))))

(define minimize-interval-list
	(lambda (lst)
		(minimize-interval-list-helper (minimize-interval-list-helper (sort-lists lst)))))

; Problem 2
(define exists?
	(lambda (pred ls)
		(if (null? ls)
			#f
			(if (pred (car ls))
				#t
				(exists? pred (cdr ls))))))

; Problem 3
(define list-index-helper
	(lambda (pred ls index)
		(if (null? ls)
			#f
			(if (pred (car ls))
				index
				(list-index-helper pred (cdr ls) (+ 1 index))))))

(define list-index
	(lambda (pred ls)
		(list-index-helper pred ls 0)))

; Problem 4
; fact and choose pulled from 2.ss
(define fact
	(lambda (n)
		(if (eq? n 0)
			1
			(* n (fact (- n 1))))))

(define choose
	(lambda (m n)
		(/ (fact m) (* (fact n) (fact (- m n))))))

(define create-row
	(lambda (rownum index)
		(if (= rownum index)
			'(1)
			(cons (choose rownum index) (create-row rownum (+ 1 index))))))

(define pascal-triangle
	(lambda (n)
		(if (negative? n)
			'()
			(if (= 0 n)
				'((1))
				(cons (create-row n 0) (pascal-triangle (- n 1)))))))



; Problem 5
; Needed help on this one
(define product
	(lambda (set1 set2)
		(if (or (null? set1) (null? set2))
			'()
			(if (null? (cdr set1))
				(product-helper (car set1) set2)
				(append (product-helper (car set1) set2) (product (cdr set1) set2))))))

(define product-helper
	(lambda (first ls)
		(if (null? (cdr ls))
			(list (list first (car ls)))
			(cons (list first (car ls)) (product-helper first (cdr ls))))))

; Problem 6
(define max-edges
	(lambda (n)
		(/ (* n (sub1 n)) 2)))

; Problem 7
; Create a new list of lists that have the vertex cons into the 
; list of edges for each element.
; Then check to see if they are all the same size as the overall list
(define complete?
	(lambda (gr)
		(complete-helper? gr (create-new-list gr))))

(define complete-helper?
	(lambda (gr lol)
		(if (null? lol)
			#t
			(if (= (length gr) (length (car lol)))
				(complete-helper? gr (cdr lol))
				#f))))

(define create-new-list
	(lambda (gr)
		(if (null? gr)
			'()
			(cons (cons (caar gr) (cadar gr)) (create-new-list (cdr gr))))))

; Problem 8
(define complete
	(lambda (ls)
		(complete-helper ls ls)))

(define complete-helper
	(lambda (og ls)
		(if (null? ls)
			'()
			(cons (create-vertex (car ls) og) (complete-helper og (cdr ls))))))
(define create-vertex
	(lambda (vert og)
		(list vert (remove-first vert og))))
; Problem 9
(define replace
	(lambda (old new ls)
		(if (null? ls)
			'()
			(if (eq? (car ls) old)
				(cons new (replace old new (cdr ls)))
				(cons (car ls) (replace old new (cdr ls)))))))

(define remove-first
	(lambda (c ls)
		(if (null? ls)
			'()
			(if (eq? (car ls) c)
				(append '() (cdr ls))
				(cons (car ls) (remove-first c (cdr ls)))))))

(define remove-last
	(lambda (c ls)
		(reverse (remove-first c (reverse ls)))))