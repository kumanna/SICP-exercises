(define (product1 term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc i) (+ i 1))
(define (factorial1 n)
  (cond ((< n 0) 0)
	((< n 1) 1)
	(else (product1 identity 1 inc n))))
(display (factorial1 5))
(newline)

(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product2 term (next a) next b))))

(define (factorial2 n)
  (cond ((< n 0) 0)
	((< n 1) 1)
	(else (product2 identity 1 inc n))))

(display (factorial2 5))
(newline)
