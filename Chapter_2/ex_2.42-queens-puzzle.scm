(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (enumerate-interval n1 n2)
  (define (enumerate-interval-iter start stop current)
    (if (< stop start)
	'()
	(if (= start stop)
	    (append current (list start))
	    (enumerate-interval-iter (+ 1 start) stop (append current (list start))))))
  (enumerate-interval-iter n1 n2 '()))

(define empty-board '())

(define (get-row positions column)
  (if (= column 1) 
      (car positions) 
      (get-row (cdr positions) (- column 1))))

;; Creates a list of pairs of column indexes consisting of the kth 
;; column and all other columns 
(define (last-column-pairs k) 
  (map (lambda (j) (list j k)) 
       (enumerate-interval 1 (- k 1))))

(define (diagonal? k positions)
  (map (lambda (pair)
	 (= (abs (- (get-row positions (car pair))
		    (get-row positions (cadr pair))))
	    (abs (- (car pair) (cadr pair)))))
       (last-column-pairs k)))

(define (same-row? k positions)
  (map (lambda (pair)
	 (= (get-row positions (car pair))
	    (get-row positions (cadr pair))))
       (last-column-pairs k)))

(define (safe-row? k positions)
  (not (accumulate (lambda (x y) (or x y)) #f (same-row? k positions))))

(define (safe-diagonal? k positions)
  (not (accumulate (lambda (x y) (or x y)) #f (diagonal? k positions))))

(define (safe? k positions)
  (and (safe-row? k positions)
       (safe-diagonal? k positions)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flatmap
		 (lambda (rest-of-queens)
		   (map (lambda (new-row)
			  (adjoin-position new-row k rest-of-queens))
			(enumerate-interval 1 board-size)))
		 (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (adjoin-position 2 4 (list 1 2 3)))
(newline)
(display (queens 4))
(newline)
