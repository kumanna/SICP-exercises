(define n-called 0)

(define (f n)
  (set! n-called (+ 1 n-called))
  (if (and (= n-called 1) (= n 0))
      1
      0))

(display (+ (f 0) (f 1)))
(newline)
(set! n-called 0)
(display (+ (f 1) (f 0)))
(newline)
