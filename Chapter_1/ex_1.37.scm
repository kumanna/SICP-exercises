(define (cont-frac-iter numerator denominator k value)
  (if (= k 0)
      value
      (cont-frac-iter numerator
      		      denominator
      		      (- k 1)
      		      (/ (numerator k) (+ (denominator k) value)))))

(display (cont-frac-iter (lambda (i) 1.0)
			 (lambda (i) 1.0)
			 10
			 0.0))
(newline)
(define (eminus2-denominator k)
  (let ((r (remainder (+ 1 k) 3)))
    (if (= r 0)
	(* 2 (/ (+ k 1) 3))
	1)))

(display (+ 2 (cont-frac-iter (lambda (i) 1.0)
			      eminus2-denominator
			      10
			      0.0)))

(newline)
(define (tan-cf x k)
  (cont-frac-iter
   (lambda (i) (if (= i 1)
		   x
		   (* (* -1 x) x)))
   (lambda (i) (- (* 2 i) 1))
   k
   0.0))
(display (tan-cf (/ 3.14159265 6) 10))
(newline)

(define (cont-frac numerator denominator k)
  (define (cont-frac-recursive i)
    (if (> i k)
	0
	(/ (numerator i) (+ (denominator i) (cont-frac-recursive (+ 1 i))))))
  (cont-frac-recursive 1))

(define (tan-cf2 x k)
  (cont-frac
   (lambda (i) (if (= i 1)
		   x
		   (* (* -1 x) x)))
   (lambda (i) (- (* 2 i) 1))
   k))

(display (tan-cf2 (/ 3.14159265 6) 10))
(newline)
