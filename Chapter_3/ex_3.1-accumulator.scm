(define (make-accumulator total)
  (lambda (amount)
    (begin (set! total (+ total amount)) total)))

(define A (make-accumulator 5))
(display (A 10))
(newline)
(display (A 10))
(newline)
