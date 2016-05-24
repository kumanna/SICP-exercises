(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse1 s)
  (fold-right (lambda (x y) (append y (list x))) '() s))

(define (reverse2 s)
  (fold-left (lambda (x y) (append (list y) x)) '() s))

(display (reverse1 (list 1 2 3 4)))
(newline)

(display (reverse2 (list 1 2 3 4)))
(newline)
