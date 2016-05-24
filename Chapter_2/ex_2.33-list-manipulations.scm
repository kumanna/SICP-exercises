(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (mymap p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y)
		)
	      '() sequence))

(display (mymap (lambda (x) (+ x 2)) (list 1 2 3 4)))
(newline)

(define (myappend seq1 seq2)
  (accumulate cons
	      seq2
	      seq1
	      ))
(display (myappend (list 1 2 3) (list 10 20 30)))
(newline)

(define (mylength seq)
  (accumulate (lambda (x y)
		(if (not (null? x))
		    (+ y 1)
		    y))
	      0
	      seq))

(display (mylength (myappend (list 1 2 3) (list 10 20 30))))
(newline)
