(define (last-pair li)
  (define (last-pair-iter l p)
    (if (null? l)
	p
	(last-pair-iter (cdr l) (car l))))
  (last-pair-iter li '()))
(display (last-pair (list 1 2 3)))
(newline)
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(define (reverse li)
  (define (reverse-iter l r)
    (if (null? l)
	r
	(reverse-iter (cdr l) (append (list (car l)) r))))
  (reverse-iter li (list)))
(display (reverse (list 1 2 3 4)))
(newline)
