(define (test-vector-append-list)
    (let ([correct '(
		     #(a b c d)
		     #(1 2 3 4 5 6)
		     #(a)
		     #(a)
		     #()
		     #(a b (c d e))
		     )]
          [answers 
            (list 
	     (vector-append-list (quote #(a b)) (quote (c d)))
	     (vector-append-list (quote #(1 2 3 4)) (quote (5 6)))
	     (vector-append-list (quote #(a)) (quote ()))
	     (vector-append-list (quote #()) (quote (a)))
	     (vector-append-list (quote #()) (quote ()))
	     (vector-append-list (quote #(a b)) (quote ((c d e))))
	     )])
      (display-results correct answers equal?)))

(define (test-qsort)
    (let ([correct '(
		     ()
		     (9 7 5 4 2)
		     (6 5 4 3 2 1)
		     (6 5 5 4 3 2 1 1)
		     ("rose-hulman" "great" "day" "at" "a")
		     )]
          [answers 
            (list 
	     (qsort < (quote ()))
	     (qsort > (quote (7 2 5 9 4)))
	     (qsort > (quote (6 3 2 4 1 5)))
	     (qsort > (quote (1 6 5 4 3 2 1 5)))
	     (qsort string>? (quote ("a" "great" "day" "at" "rose-hulman")))
	     )])
      (display-results correct answers equal?)))

(define (test-connected?)
    (let ([correct '(
		     #t
		     #t
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (and (connected? (quote ((a ())))) 
		  (not (connected? (quote ((a ()) (b ()))))))
	     (and (connected? (quote ((a (b)) (b (a))))) 
		  (not (connected? (quote ((a (b)) (c ()) (b (a)))))))
	     (and (connected? (quote ((a (b c)) (b (c a)) (c (b a))))) 
		  (not (connected? (quote ((a (b c)) (b (c a)) (c (b a)) 
					   (d (e f g)) (e (d f g)) (f (d e g)) 
					   (g (d e f)))))))
	     (and (not (connected? (quote ((a (b)) (b (a)) (c (d)) (d (c)))))) 
		  (connected? (quote ((a (b)) (k (j)) (j (k i)) (c (b d)) 
				      (i (h j)) (h (i g)) (g (f h)) (b (c a)) 
				      (f (e g)) (e (d f)) (d (c e))))))
	     (and (connected? (quote ((a ())))) 
		  (not (connected? (quote ((a (b c)) (b (a c)) (c (b a)) 
					   (d (e f)) (e (d f)) (f (d e)))))))
	     )])
      (display-results correct answers equal?)))

(define (test-reverse-it)
    (let ([correct '(
		     ()
		     (1)
		     (5 4 3 2 1)
		     )]
          [answers 
            (list 
	     (reverse-it '())
	     (reverse-it '(1))
	     (reverse-it '(1 2 3 4 5))
	     )])
      (display-results correct answers equal?)))

(define (test-BST)
    (let ([correct '(
		     #t
		     ()
		     (1)
		     (1 2)
		     (1 2 3)
		     (1 3 4 5 6 7 8 9)
		     (1 3 4 5 6 7 8 9)
		     8
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (empty-BST? (empty-BST))
	     (BST-inorder (empty-BST))
	     (BST-inorder (BST-insert 1 (empty-BST)))
	     (BST-inorder (BST-insert 1 (BST-insert 2 (empty-BST))))
	     (BST-inorder (BST-insert 2 (BST-insert 1 (BST-insert 3 (empty-BST)))))
	     (BST-inorder (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 8)))
	     (BST-inorder (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 6 8)))
	     (BST-element (BST-left (BST-right (BST-right 
		 (BST-insert-nodes (empty-BST) '(4 7 3 9 6 1 5 8))))))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 8))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      10)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 1))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      2)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST-contains? (empty-BST) 1)) 
		  (BST-contains? (BST-insert-nodes (empty-BST) 
						   '(4 7 3 9 6 1 5 8)) 
				 4))
	     (and (not (BST-contains? (BST-insert-nodes (empty-BST) 
							'(4 7 3 9 6 1 5 8)) 
				      0)) 
		  (BST-contains? (BST-insert 1 (empty-BST)) 1))
	     (and (not (BST? #t)) (BST? '()))
	     (and (not (BST? '(1))) (BST? '()))
	     (and (not (BST? '(1 2 3))) (BST? '()))
	     (and (not (BST? '(1 ()))) (BST? '()))
	     (and (BST? '(1 () ())) (BST? '()))
	     (and (not (BST? '(a () ()))) (BST? '()))
	     (and (not (BST? '(1 (2 () ()) ()))) (BST? '()))
	     (and (not (BST? #t)) (BST? '(1 () (3 (2 () ()) ()))))
	     (and (not (BST? '(4 () (6 (3 () ()) ())))) (BST? '()))
	     )])
      (display-results correct answers equal?)))

(define (test-map-by-position )
    (let ([correct '(
		     (2 2 2 2)
		     )]
          [answers 
            (list 
	     (map-by-position (list cadr - length (lambda(x)(- x 3))) 
			      '((1 2) -2 (3 4) 5))
	     )])
      (display-results correct answers equal?)))

(define (test-bt-leaf-sum)
    (let ([correct '(
		     -4
		     5
		     16
		     21
		     55
		     )]
          [answers 
            (list 
	     (bt-leaf-sum -4)
	     (bt-leaf-sum '(a 2 3))
	     (bt-leaf-sum '(a 5 (b 4 7)))
	     (bt-leaf-sum '(m (l (h 0 (u 1 2)) 
				 3) 
			      (n (a 4 5 ) 
				 6)))
	     (bt-leaf-sum '(l (s (r (f 0 
				       (i 1 2)) 
				    3) 
				 (t 4 
				    (c 5 6)))
			      (s (a 7 
				    (s 8 9)) 
				 10)))
	     )])
      (display-results correct answers equal?)))

(define (test-bt-inorder-list)
    (let ([correct '(
		     ()
		     (a)
		     (h u l m a n)
		     (f i r s t c l a s s)
		     )]
          [answers 
            (list 
	     (bt-inorder-list 0)
	     (bt-inorder-list '(a 4 5))
	     (bt-inorder-list '(m (l (h 0 
					(u 1 2)) 
				     3) 
				  (n (a 4 5 ) 
				     6)))
	     (bt-inorder-list '(l (s (r (f 0 
					   (i 1 2)) 
					3) 
				     (t 4 
					(c 5 6)))
				  (s (a 7 
					(s 8 9)) 
				     10)))

	     )])
      (display-results correct answers equal?)))

(define (test-bt-max)
    (let ([correct '(
		     -1
		     3
		     7
		     100
		     1200
		     )]
          [answers 
            (list 
	     (bt-max -1)
	     (bt-max '(a 2 3))
	     (bt-max '(a 5 (b 4 7)))
	     (bt-max '(m (l (h 100 (u 1 2)) 3) (n (a 4 5 ) 6)))
	     (bt-max '(l (s (r (f 0 (i 1 2)) 3) (t 4 (c 1200 6))) (s (a 7 (s 8 9)) 10)))
	     )])
      (display-results correct answers equal?)))

(define (test-bt-max-interior)
    (let ([correct '(
		     a
		     b
		     b
		     c
		     a
		     )]
          [answers 
            (list 
	     (bt-max-interior '(a -5 -4))
	     (bt-max-interior '(a (b 1 2) -4))
	     (bt-max-interior '(a (b -1 -2) (c -2 -2)))
	     (bt-max-interior '(a (b (c (d (e 3 2) -1) 4) -2) (f 0 (g 0 1))))
	     (bt-max-interior '(b (a -3000 -4000) -2000))
	     )])
      (display-results correct answers equal?)))


(define (test-bt-max-interior-harder)
    (let ([correct '(
		     ak
		     ar
		     br
		     ad
		     ah
		     al
		     an
		     )]
          [answers 
            (list 
	    (bt-max-interior 
	     '(aa (ab -7
		      (ac 0 (ad (ae (af -2 -10) 
				    (ag 1 -9)) 
				(ah 5 
				    (ai -3 -12)))))
		  (aj -1
		      (ak (al (am (an -4 -8) 
				  (ao -5 12)) 
			      (ap (aq 2 11) 
				  (ar 7 6)))
			  (as 9 10)))))
	    	(bt-max-interior 
		 '(aa (ab (ac (ad (ae (af -25 62) 
				      (ag -4 -5))
				  (ah (ai -1 -41) 
				      -54))
			      (aj (ak (al 35 8) 
				      (am -55 59)) 
				  (an (ao -77 76) 
				      (ap -9 48))))
			  (aq (ar (as (at 5 74) 
				      (au -26 0))
				  (av (aw 51 -56) 
				      (ax 39 70)))
			      (ay (az -2 
				      (ba 58 -47)) 
				  (bb (bc -11 -76) 
				      (bd -37 -15)))))
		      (be -20 55)))
		(bt-max-interior 
		 '(aa (ab (ac (ad (ae (af (ag 5 74) 
					  37) 
				      31)
				  (ah (ai (aj 29 -34) 
					  (ak -67 18)) 
				      (al (am -41 27) 16)))
			      (an (ao (ap (aq 2 -12) 
					  (ar -66 59))
				      (as (at -27 -59) 
					  (au -32 43)))
				  (av (aw (ax -56 -2) 
					  (ay 36 40))
				      (az (ba -65 76) 
					  (bb -48 -47)))))
			  (bc (bd (be (bf 15 
					  (bg 75 -77)) 
				      (bh (bi -24 -45) 
					  (bj 64 1)))
				  (bk (bl (bm -70 -3) 
					  (bn -71 35)) 
				      45))
			      (bo -78 47)))
		      (bp (bq (br (bs (bt (bu 39 49) 
					  (bv -11 63))
				      (bw (bx 48 -42) 
					  (by 25 33)))
				  (bz (ca (cb 38 66) 
					  (cc -40 -37))
				      (cd (ce 26 30) 
					  (cf -26 58))))
			      (cg 28 -39))
			  (ch (ci (cj (ck (cl -15 -31) 
					  (cm -62 32))
				      (cn (co -54 -57) 
					  (cp 13 -73)))
				  (cq (cr (cs 68 62) 
					  42) 
				      65))
			      (ct (cu (cv (cw -8 -49) 
					  (cx 24 -60))
				      (cy (cz 73 -5) 
					  (da 46 -43)))
				  20)))))
			(bt-max-interior 
			 '(aa (ab (ac (ad 74 46) 
				      (ae 26 -62))
				  (af (ag 23 -27) 
				      (ah -32 15)))
			      -9))
			(bt-max-interior 
			 '(aa (ab (ac (ad 43 -46) 
				      (ae 9 -69))
				  (af (ag -76 32) 
				      (ah 64 7)))
			      (ai (aj (ak -55 -24) 
				      (al 21 -2))
				  (am (an -50 -30) 
				      (ao -35 -58)))))
			(bt-max-interior
			 '(aa (ab (ac (ad -46 -22) 
				      (ae 1 -51)) 
				  (af -78 
				      (ag -26 -59)))
			      (ah (ai (aj 65 -52) 
				      (ak -73 -28)) 
				  (al 22 
				      (am 51 2)))))
				(bt-max-interior
				 '(aa (ab (ac (ad (ae 24 -75) 
						  (af 55 34))
					      (ag (ah 15 -78) 
						  (ai -31 -11)))
					  (aj (ak (al 40 -55) 
						  (am 48 -21)) 
					      -77))
				      (an (ao (ap -76 
						  (aq 41 67)) 
					      (ar 50 (as 60 -27)))
					  (at 16 
					      (au (av 62 -58) 
						  (aw -9 72))))))
			)])
      (display-results correct answers equal?)))


;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'vector-append-list) 
  (test-vector-append-list)
  (display 'qsort) 
  (test-qsort)    
  (display 'connected?) 
  (test-connected?)
  (display 'reverse-it ) 
  (test-reverse-it)  
  (display 'BST) 
  (test-BST)  
  (display 'map-by-position) 
  (test-map-by-position )
  (display 'bt-leaf-sum) 
  (test-bt-leaf-sum)  
  (display 'bt-inorder-list) 
  (test-bt-inorder-list)
  (display 'bt-max) 
  (test-bt-max)
  (display 'bt-max-interior) 
  (test-bt-max-interior)
  (display 'bt-max-interior-harder) 
  (test-bt-max-interior-harder)
)

(define r run-all)
