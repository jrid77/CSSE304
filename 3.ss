(define nearest-point
	(lambda (p lop)
		(if (null? (cdr lop))
			(car lop)
			(if (<= (distance p (car lop)) (distance p (nearest-point p (cdr lop))))
				(car lop)
				(nearest-point p (cdr lop))))))

(define union
	(lambda (set1 set2)
		(if (null? set2)
			set1
			(if (member (car set2) set1)
				(union set1 (cdr set2))
				(cons (car set2) (union set1 (cdr set2)))))))

(define intersection
	(lambda (set1 set2)
		(if (null? set1)
			'()
			(if (member (car set1) set2)
				(cons (car set1) (intersection (cdr set1) set2))
				(intersection (cdr set1) set2)))))

(define subset?
	(lambda (s1 s2)
		(if (null? s1)
			#t
			(if (member (car s1) s2)
				(subset? (cdr s1) s2)
				#f))))


; check if set
; check if all are list
; check if size 2
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

(define domain
	(lambda (r)
		(if (null? r)
			'()
			(if (member (caar r) (domain (cdr r)))
				(domain (cdr r))
				(cons (caar r) (domain (cdr r)))))))

(define reflexive-s
	(lambda (list1 list2)
		(if (null? list1)
			#t
			(if (member (list (caar list1) (caar list1)) list2)
				(reflexive-s (cdr list1) list2)
				#f))))

(define reflexive-f
	(lambda (list1 list2)
		(if (null? list1)
			#t
			(if (member (list (cadar list1) (cadar list1)) list2)
				(reflexive-f (cdr list1) list2)
				#f))))
; I got help from 
(define reflexive?
	(lambda (l)
		(and (reflexive-f l l) (reflexive-s l l))))

(define hailstone-step-count
	(lambda (n)
		(hailstone-helper n 0)))

(define hailstone-helper
	(lambda (n count)
		(cond 
			[(= 1 n) count]
			[(= 0 (modulo n 2)) (hailstone-helper (/ n 2) (+ 1 count))]
			[else (hailstone-helper (+ 1 (* 3 n)) (+ 1 count))])))



; These are pulled from 2.ss
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define vec-length
	(lambda (v)
		(sqrt (dot-product v v))))

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