(define (my-fast-mult-iter b n a)
  (cond ((= n 1) (+ b a))
	((even? n) (my-fast-mult-iter (* 2 b) (/ n 2) a))
	(else (my-fast-mult-iter b (- n 1) b))))

(define (my-fast-mult b n)
  (my-fast-mult-iter b n 0))

(display (my-fast-mult 32 32))
(newline)
(display (my-fast-mult 8 4))
(newline)
