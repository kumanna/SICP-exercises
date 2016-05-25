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
  (if (> n1 n2)
      '()
      (append (list n1) (enumerate-interval (+ n1 1) n2))))

(display (enumerate-interval 2 10))
(newline)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (number-pairs n1 n2)
  (flatmap (lambda (i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1)))) (enumerate-interval n1 n2)))

(define (make-pair-sum pair)
  (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))


(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) #t)
	  ((= 0 (remainder x divisor)) #f)
	  (else (test (+ divisor 1)))))
  (test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (number-pairs 1 n))))

(define (all-number-triplets n)
  (flatmap (lambda (i) (map (lambda (j) (append j (list i))) (number-pairs 1 (- i 1)))) (enumerate-interval 1 n)))

(define (unique-number-triplets n)
  (filter (lambda (triplet)
	    (let ((x (car triplet))
		  (y (car (cdr triplet)))
		  (z (car (cdr (cdr triplet)))))
	    (and (< x y) (< y z)))) (all-number-triplets n)))

(display (unique-number-triplets 5))
(newline)

(define (triplet-sums n s)
  (filter (lambda (triplet)
	    (let ((x (car triplet))
		  (y (car (cdr triplet)))
		  (z (car (cdr (cdr triplet)))))
	    (= s (+ x y z)))) (unique-number-triplets n)))

(display (triplet-sums 10 12))
(newline)	 
