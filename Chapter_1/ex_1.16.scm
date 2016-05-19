(define (my-fast-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (my-fast-expt-iter (* b b) (/ n 2) a))
	(else (my-fast-expt-iter b (- n 1) (* a b)))))

(define (my-fast-expt b n)
  (my-fast-expt-iter b n 1))

(display (my-fast-expt 3 3))
(newline)
(display (my-fast-expt 8 4))
(newline)
