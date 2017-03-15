; This works tail recursively so that it does not create more than one stack 
; frome at a time
(define fact-tail
	(lambda (n accum)
		(if (zero? n)
			accum
			(fact-tail (- n 1) (* n accum)))))

; Renaming can break things very easily
; This is fixed by using inner helper functions
(define fact-tail
	(letrec ([fact-help (lambda (n accum)
		(if (zero? n)
			accum
			(fact-help (- n 1) (* n accum))))])
	(lambda (n) (fact-help n 1))))