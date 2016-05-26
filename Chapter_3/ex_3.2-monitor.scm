(define (make-monitored f)
  (define n-called 0)
  (lambda (x)
    (if (eq? x 'how-many-calls?)
	n-called
	(begin (set! n-called (+ n-called 1)) (f x)))))
	
(define s (make-monitored sqrt))
(display (s 4))
(newline)
(display (s 9))
(newline)
(display (s 'how-many-calls?))
(newline)
