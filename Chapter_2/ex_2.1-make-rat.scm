(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (make-rat num den)
  (let ((mygcd ((if (< den 0) - +) (gcd num den))))
    (cons (/ num mygcd) (/ den mygcd))))
  
(print-rat (make-rat 4 6))
(print-rat (make-rat 4 -6))
(print-rat (make-rat -4 6))
(print-rat (make-rat -4 -6))
