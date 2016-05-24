(define (deep-reverse li)
  (if (pair? li)
      (append (deep-reverse (cdr li))
	      (list (deep-reverse (car li))))
      li))

(define x (list (list 1 2) (list 3 4 (list 5 6))))

(display (reverse x))
(newline)
(display (deep-reverse x))
(newline)
