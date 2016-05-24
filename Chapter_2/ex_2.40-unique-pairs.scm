(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval n1 n2)
  (define (enumerate-interval-iter start stop current)
    (if (< stop start)
	'()
	(if (= start stop)
	    (append current (list start))
	    (enumerate-interval-iter (+ 1 start) stop (append current (list start))))))
  (enumerate-interval-iter n1 n2 '()))

(display (enumerate-interval 0 10))
(newline)
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) #t)
	  ((= 0 (remainder x divisor)) #f)
	  (else (test (+ divisor 1)))))
  (test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))

(define (make-pair-sum pair)
  (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))

(display (make-pair-sum (list 1 2)))
(newline)

(define (number-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list j i))
		  (enumerate-interval 1 (- i 1))
		  ))
	   (enumerate-interval 1 n)))

(display "Number pairs\n")
(display (number-pairs 10))
(newline)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (number-pairs i))
		(enumerate-interval 1 n)))))
(display (prime-sum-pairs 10))
(newline)
