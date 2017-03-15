(define fact
	(lambda (n)
		(if (eq? n 0)
			1
			(* n (fact (- n 1))))))

(define choose
	(lambda (m n)
		(/ (fact m) 
		   (* (fact n) (fact (- m n))))))

(define range
	(lambda (start end)
		(if (< start end)
			(cons start (range (+ start 1) end))
			'())))

(define sum-of-squares
	(lambda (lon)
		(if (null? lon)
			0
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))))

(define make-vec-from-points
	(lambda (p1 p2)
		(if (null? p1)
			'()
			(cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2))))))

(define dot-product
	(lambda (v1 v2)
		(if (null? v1)
			0
			(+ (* (car v2) (car v1)) (dot-product (cdr v1) (cdr v2))))))

(define vec-length
	(lambda (v)
		(sqrt (dot-product v v))))

(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define cross-product-helper
	(lambda (a b c d)
		(- (* a d) (* b c))))

(define cross-product
	(lambda (v1 v2)
		(list 
			(cross-product-helper (cadr v1) (caddr v1) (cadr v2) (caddr v2))
			(- 0 (cross-product-helper (car v1) (caddr v1) (car v2) (caddr v2)))
			(cross-product-helper (car v1) (cadr v1) (car v2) (cadr v2)))))

(define parallel?
	(lambda (v1 v2)
		(equal? (sum-of-squares (cross-product v1 v2)) 0)))


;; Needed some help from friends on this one
;; Was not sure what the map function did until this one
(define collinear-helper
	(lambda (l)
		(and (equal? (car l) (cadr l)) (equal? (cadr l) (caddr l)))))

(define collinear? 
	(lambda (p1 p2 p3)
		(collinear-helper (map / (map - p2 p1) (map - p3 p2)))))

;; Also needed help on this one
;; Talked to one of my friends who has already taken the class Jacob Laird
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