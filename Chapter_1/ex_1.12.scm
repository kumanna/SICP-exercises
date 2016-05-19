(define (pascal-triangle-element row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal-triangle-element (- row 1) (- col 1)) (pascal-triangle-element (- row 1) col))))
(display (pascal-triangle-element 1 1))
(newline)
(display (pascal-triangle-element 2 1))
(display " ")
(display (pascal-triangle-element 2 2))
(newline)
(display (pascal-triangle-element 3 1))
(display " ")
(display (pascal-triangle-element 3 2))
(display " ")
(display (pascal-triangle-element 3 3))
(newline)
(display (pascal-triangle-element 4 1))
(display " ")
(display (pascal-triangle-element 4 2))
(display " ")
(display (pascal-triangle-element 4 3))
(display " ")
(display (pascal-triangle-element 4 4))
(newline)
(display (pascal-triangle-element 5 1))
(display " ")
(display (pascal-triangle-element 5 2))
(display " ")
(display (pascal-triangle-element 5 3))
(display " ")
(display (pascal-triangle-element 5 4))
(display " ")
(display (pascal-triangle-element 5 5))
(newline)
(display (pascal-triangle-element 6 1))
(display " ")
(display (pascal-triangle-element 6 2))
(display " ")
(display (pascal-triangle-element 6 3))
(display " ")
(display (pascal-triangle-element 6 4))
(display " ")
(display (pascal-triangle-element 6 5))
(display " ")
(display (pascal-triangle-element 6 6))
(newline)