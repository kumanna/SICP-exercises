(define (accumulate1 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum1 term a next b)
  (accumulate1 + 0 term a next b))

(define (product1 term a next b)
  (accumulate1 * 1 term a next b))

(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate2 combiner null-value term (next a) next b))))

(define (sum2 term a next b)
  (accumulate2 + 0 term a next b))

(define (product2 term a next b)
  (accumulate2 * 1 term a next b))

(define (identity x) x)
(define (inc i) (+ i 1))

(define (factorial1 n)
  (cond ((< n 0) 0)
	((< n 1) 1)
	(else (product1 identity 1 inc n))))
(display (factorial1 5))
(newline)

(define (factorial2 n)
  (cond ((< n 0) 0)
	((< n 1) 1)
	(else (product2 identity 1 inc n))))

(display (factorial2 5))
(newline)
