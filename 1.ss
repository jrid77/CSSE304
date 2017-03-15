(define Fahrenheit->Celsius 
	(lambda (temperature)
		(* (/ 5 9) (- temperature 32))))

(define interval-contains?
	(lambda (interval number)
		(and (<= (car interval) number) (>= (cadr interval) number))))

(define interval-intersects? 
	(lambda (i1 i2)
		(or (or (interval-contains? i1 (car i2)) (interval-contains? i1 (cadr i2))) 
			(or (interval-contains? i2 (car i1)) (interval-contains? i2 (cadr i1))))))

(define interval-union 
	(lambda (i1 i2)
		(if (interval-intersects? i1 i2)
			(list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))
			(list i1 i2))))

(define divisible-by-7? 
	(lambda (n)
		(eq? (modulo n 7) 0)))

(define ends-with-7?
	(lambda (n)
		(eq? (modulo n 10) 7)))

(define 1st
	(lambda (list)
		(car list)))


(define 2nd
	(lambda (list)
		(cadr list)))


(define 3rd
	(lambda (list)
		(caddr list)))