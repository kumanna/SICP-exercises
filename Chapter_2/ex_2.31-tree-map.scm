(define (tree-map f l)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       l))

(define (square-tree l)
  (tree-map (lambda (x) (* x x)) l))

(display (square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))))
(newline)
