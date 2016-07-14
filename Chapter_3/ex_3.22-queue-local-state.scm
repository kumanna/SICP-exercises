(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with empty queue!")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE called with an empty queue!"))
	    (else
	     (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'print-queue) print-queue)
	    (else (error "Unknown operation on queue: " m))))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'print-queue))
(newline)
((q1 'print-queue))
(newline)
((q1 'insert-queue!) 'b)
((q1 'print-queue))
(newline)
((q1 'delete-queue!))
((q1 'print-queue))
(newline)
