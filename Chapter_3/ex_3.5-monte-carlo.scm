(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random x)::float (* x (java.lang.Math:random)))

(define (random-in-range x1 x2)
  (+ x1 (* (- x2 x1) (random 1.0))))

(define (estimate-integral P x1 x2 y1 y2 n-trials)
   (define (experiment)
     (P (random-in-range x1 x2)
        (random-in-range y1 y2)))
   (monte-carlo n-trials experiment))

(define (circle-test x y)
  (< (+ (* x x) (* y y)) 1))
(display (* 4.0 (estimate-integral circle-test -1.0 1.0 -1.0 1.0 10000)))
(newline)
