(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
	 (* 2 (f1 (- n 2)))
	 (* 3 (f1 (- n 3))))))
      

(define (f2-iter n nm3 nm2 nm1)
  (cond ((< n 0) n)
	((= n 0) nm3)
	(else (f2-iter (- n 1) nm2 nm1 (+ nm1 (* 2 nm2) (* 3 nm3))))))

(define (f2 n) (f2-iter n 0 1 2))

(display (f1 4))
(newline)
(display (f2 4))
(newline)
