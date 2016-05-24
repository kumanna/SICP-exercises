(define (fringe li)
  (if (pair? li)
      (append (fringe (car li))
	      (fringe (cdr li)))
      (if (not (null? li)) (list li) '())))
(define x (list (list 1 2) (list 3 4)))
(display (fringe x))
(newline)
(display (fringe (list x x)))
(newline)