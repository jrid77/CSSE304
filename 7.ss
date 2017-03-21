; Problem 1
(define copy-from-vector
	(lambda (new-vec v index)
		(if (= index (vector-length v))
			new-vec
			(begin
				(vector-set! new-vec index (vector-ref v index))
				(copy-from-vector new-vec v (+ 1 index))))))

(define copy-from-list
	(lambda (new-vec lst index)
		(if (null? lst)
			new-vec
			(begin
				(vector-set! new-vec index (car lst))
				(copy-from-list new-vec (cdr lst) (+ 1 index))))))

(define vector-append-list
	(lambda (v lst)
		(let ([new-vector (make-vector (+ (vector-length v) (length lst)))])
			(copy-from-vector new-vector v 0)
			(copy-from-list new-vector lst (vector-length v))
			new-vector)))

; Problem 2
(define qsort-helper
	(lambda (pred ls)
		(if (null? ls)
			'()
			(if (pred (car ls))
				(cons (car ls) (qsort-helper pred (cdr ls)))
				(qsort-helper pred (cdr ls))))))

(define qsort
	(lambda (pred ls)
		(if (null? ls)
			'()
			(append
			 (qsort pred (qsort-helper (lambda (x) (pred x (car ls))) (cdr ls)))
			 (list (car ls)) 
			 (qsort pred (qsort-helper (lambda (x) (not (pred x (car ls)))) (cdr ls)))))))
; Problem 3
; Got alot of help from Tong(Jake) Liu
; 1: Flatten List
; 2: Sort based on first element
; 3: Iterate and check intersects

(define sort-g
	(lambda (g)
		(list-sort (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y)))) g)))

(define flatten
	(lambda (g)
		(map cons (map car g) (map cadr g))))

(define intersects?
	(lambda (ls1 ls2)
		(if (null? ls1)
			#f
			(if (member (car ls1) ls2)
				#t
				(intersects? (cdr ls1) ls2)))))

(define union
	(lambda (ls1 ls2)
		(if (null? ls2)
			ls1
			(if (member (car ls2) ls1)
				(union ls1 (cdr ls2))
				(union (cons (car ls2) ls1) (cdr ls2))))))

(define connected-helper?
	(lambda (g)
		(if (null? (cdr g))
			#t
			(if (intersects? (car g) (cadr g))
				(connected-helper? (cons (union (car g) (cadr g)) (cddr g)))
				#f))))

(define connected?
	(lambda (g)
		(connected-helper? (sort-g (flatten g)))))

; Problem 4
(define reverse-helper
	(lambda (lst acc)
		(if (null? (cdr lst))
			(cons (car lst) acc)
			(reverse-helper (cdr lst) (cons (car lst) acc)))))

(define reverse-it
	(lambda (lst)
		(if (null? lst)
			'()
			(reverse-helper lst '()))))






; Problem 5
; BST FORMAT '(root (left subtree) (right subtree))
(define empty-BST
	(lambda ()
		'()))

(define empty-BST?
	(lambda (bst)
		(null? bst)))

(define BST-element
	(lambda (bst)
		(if (list? bst) (car bst) '())))

(define BST-left
	(lambda (bst)
		(if (list? bst) (cadr bst) '())))

(define BST-right
	(lambda (bst)
		(if (list? bst) (caddr bst) '())))



(define BST-insert
	(lambda (num bst)
		(cond 
			[(null? bst) (list num '() '())] ; Insert value
			[(= (car bst) num) bst] ; Handle same value
			[(< (car bst) num) (list (car bst) (cadr bst) (BST-insert num (caddr bst)))] ; Go right
			[(> (car bst) num) (list (car bst) (BST-insert num (cadr bst)) (caddr bst))] ; Go left
			)))

(define BST-inorder
	(lambda (bst)
		(if (null? bst)
			'()
			(append (BST-inorder (cadr bst)) (list (car bst)) (BST-inorder (caddr bst))))))

(define sorted?
	(lambda (lst)
		(if (null? (cdr lst))
			#t
			(if (< (car lst) (cadr lst))
				(sorted? (cdr lst))
				#f))))

(define BST-helper?
	(lambda (bst)
		(cond 
			[(not (list? bst)) #f]
			[(null? bst) #t]
			[else 
				[and 
				(= 3 (length bst)) 
				(list? (BST-left bst))
				(list? (BST-right bst))
				(number? (BST-element bst))
				(and (BST-helper? (BST-left bst)) (BST-helper? (BST-right bst)))
				]])))

(define BST?
	(lambda (bst)
		(if (null? bst)
			#t
			(if (BST-helper? bst) 
				(sorted? (BST-inorder bst))
				#f))))

(define BST-insert-nodes-helper
	(lambda (bst nums)
		(if (null? (cdr nums))
			(BST-insert (car nums) bst)
			(BST-insert (car nums) (BST-insert-nodes-helper bst (cdr nums))))))

(define BST-insert-nodes
	(lambda (bst nums)
			(BST-insert-nodes-helper bst (reverse nums))))

(define BST-contains?
	(lambda (bst num)
		(if (null? bst)
			#f
			(if (< (BST-element bst) num)
				(BST-contains? (BST-right bst) num)
				(if (equal? (BST-element bst) num)
					#t
					(BST-contains? (BST-left bst) num))))))


; Problem 6
(define map-by-position
	(lambda (fn-list arg-list)
		(map (lambda (f arg) (f arg)) fn-list arg-list)))

; Problem 7
(define bt-leaf-sum
	(lambda (t)
		(cond 
			[(null? t) 0]
			[(number? t) t]
			[else (+ (bt-leaf-sum (BST-left t)) (bt-leaf-sum (BST-right t)))])
		))

(define bt-inorder-list
	(lambda (t)
		(cond
			[(or (null? t) (number? t)) '()]
			[else (append 
				(bt-inorder-list (BST-left t)) 
				(list (BST-element t)) 
				(bt-inorder-list (BST-right t)))])))


(define bt-max
	(lambda (t)
		(if (number? t)
			t
			(max (bt-max (BST-left t)) (bt-max (BST-right t))))))

(define max-of-two
	(lambda (t1 t2)
		(if (> (cadr t1) (cadr t2))
			t1
			t2)))

(define max-of-three
	(lambda (l c r)
		(cond
			[(and (null? (car l)) (null? (car r))) (list (car c) (cadr c) (cadr c))]
			[(null? (car l)) (list (car (max-of-two r c)) (cadr (max-of-two r c)) (cadr c))]
			[(null? (car r)) (list (car (max-of-two l c)) (cadr (max-of-two l c)) (cadr c))]
			[(and (>= (cadr l) (cadr c)) (>= (cadr l) (cadr r))) (list (car l) (cadr l) (cadr c))]
			[(and (>= (cadr r) (cadr c)) (>= (cadr r) (cadr l))) (list (car r) (cadr r) (cadr c))]
			[else (list (car c) (cadr c) (cadr c))])))

(define bt-max-interior-helper
	(lambda (t)
		(if (number? t) 
			(list '() t t)
			(let ( 
				[left (bt-max-interior-helper (BST-left t))]
				[right (bt-max-interior-helper (BST-right t))]
			)
			
		(max-of-three 
			left
			(list 
				(BST-element t) 
				(+	(caddr left)
					(caddr right)))
			right)))))

(define bt-max-interior
	(lambda (t)
		(car (bt-max-interior-helper t))))