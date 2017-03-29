;; Test code for CSSE 304 Exam 1 201710

(define (test-max-diff)
    (let ([correct '(
		     0
		     6
		     0
		     8
		     )]
          [answers 
            (list 
	     (max-diff '(7))
	     (max-diff '(8 4 5 2))
	     (max-diff '(9 9 9 9 9))
	     (max-diff '(3 2 1 4 7 8 2 9 1))		   
	     )])
      (display-results correct answers equal?)))

(define (test-member-n-times?)
    (let ([correct '(
		     #t
		     #t
		     #f
		     #t
		     #f
		     )]
          [answers 
            (list 
	     (member-n-times? 'b '(a b c b d b d) 2)
	     (member-n-times? 'b '(a b c b d b) 3)
	     (member-n-times? 'b '(a b c b d b) 4)
	     (member-n-times? 'b '() 0)
	     (member-n-times? 'b '() 1)
	     )])
      (display-results correct answers equal?)))

(define (test-most-frequent)
    (let ([correct '(
		     #f
		     3
		     -2
		     4
		     4
		     1
		     -999
		     )]
          [answers 
            (list
	     (most-frequent '())
	     (most-frequent '(3))
	     (most-frequent '(-2 3 -2))
	     (most-frequent '(4 3 2 1 1 2 1 4 5 4 2 4 5 4 6))
	     (most-frequent '(5 4 5 3 5 2 1 1 2 1 4 5 4 2 4 5 4 6))
	     (most-frequent '(2 1 3 2 1 3 2 1 3 2 1 3))
	     (most-frequent '(-70 63 49 -82 -999 55 -999 47 -82 -999 5))
	     )])
      (display-results correct answers equal?)))

(define (test-slist-same?)
    (let ([correct 
	   '(
	     #t
	     #t
	     #f
	     #t
	     #f
	     #t
	     #t
	     #f
	     #f
	     )]
          [answers 
            (list 
	     (slist-same? '() '() eq?)
	     (slist-same? '(a) '(a) eq?)
	     (slist-same? '(a) '(b) eq?)
	     (slist-same? '(a) '(b) (lambda (x y) #t))
	     (slist-same? '(a ((c))) '(a ((c ()))) (lambda (x y) #t))
	     (slist-same? '(() a ((c()))) '(() a ((b ()))) (lambda (x y) #t))
	     (let ([first-letter 
		    (lambda (s) (car (string->list (symbol->string s))))])
	      (slist-same? '(a (b (((c) () d)))) 
			   '(ab (bb (((cb) () db))))
			   (lambda (x y) 
			     (eqv? (first-letter x) (first-letter y)))))
	    (let ([first-letter 
		   (lambda (s) (car (string->list (symbol->string s))))])
	      (slist-same? '(a (b (((c) () d)))) 
			   '(ab (bb (((cb) () bd))))
			   (lambda (x y) 
			     (eqv? (first-letter x) (first-letter y)))))
	    (let ([first-letter 
		   (lambda (s) (car (string->list (symbol->string s))))])
	      (slist-same? '(a (b (((c) () d)))) 
			   '(ab (bb (((cb) (()) db))))
			   (lambda (x y)
			     (eqv? (first-letter x) (first-letter y)))))
	     )])
      (display-results correct answers equal?)))

(define (test-make-lyst)
    (let ([correct '(
		     0
		     (2 1 2)
		     (2 1 2)
		     (4 0 4)
		     (4 0 4 4 2)
		     ((4 0 4 4 2) (5 8 7 5 -8))
		     )]
          [answers 
            (list 
	     (let ([my-list (make-lyst)])
	       (my-list 'len))
	     (let ([my-list (make-lyst)])
	       (my-list 'add-first 1)
	       (my-list 'add-last 2)
	       (list  (my-list 'len) (my-list 'first) (my-list 'last)))
	     (let ([my-list (make-lyst)])
	       (my-list 'add-last 2)
	       (my-list 'add-first 1)
	       (list  (my-list 'len) (my-list 'first) (my-list 'last)))
	     (let ([my-list (make-lyst)])
	       (my-list 'add-last 2)
	       (my-list 'add-first 1)
	       (my-list 'add-first 0)
	       (my-list 'add-last 4)
	       (list  (my-list 'len) (my-list 'first) (my-list 'last)))
	     (let ([my-list (make-lyst)])
	       (my-list 'add-last 2)
	       (my-list 'add-first 1)
	       (my-list 'add-first 0)
	       (my-list 'add-last 4)
	       (list  (my-list 'len) (my-list 'first) 
		      (my-list 'last) (my-list 'get 3) (my-list 'get 2)))
	     (let ([lyst1 (make-lyst)]
		   [lyst2 (make-lyst)])
	       (lyst1 'add-last 2)
	       (lyst1 'add-first 1)
	       (lyst1 'add-first 0)
	       (lyst1 'add-last 4)
	       (lyst2 'add-last -8)
	       (lyst2 'add-first 3)
	       (lyst2 'add-first 8)
	       (lyst2 'add-last 5)
	       (lyst2 'add-last 7)
	       (list (list  (lyst1 'len) (lyst1 'first) (lyst1 'last) 
			    (lyst1 'get 3) (lyst1 'get 2))
		     (list  (lyst2 'len) (lyst2 'first) (lyst2 'last) 
			    (lyst2 'get 3) (lyst2 'get 2))))
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
  (display 'max-diff) 
  (test-max-diff)
  (display 'member-n-times?) 
  (test-member-n-times?)
  (display 'most-frequent) 
  (test-most-frequent)
  (display 'slist-same?) 
  (test-slist-same?)    
  (display 'make-lyst) 
  (test-make-lyst)
)

(define r run-all)

