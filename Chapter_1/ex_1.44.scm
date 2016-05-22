(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f k)
  (if (= k 1)
      f
      (repeated (compose f f) (- k 1))))

(define dx 0.00001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3.0)))

(define (n-fold-smoothed f n)
  (repeated (smooth f) n))

(display ((n-fold-smoothed (lambda (x) (* x x)) 3) 1.0))
(newline)
