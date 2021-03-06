;; This just uses lists as keys and doesn't quite generalize the table to higher dimensions
(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (assoc same-key? key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc same-key? key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc same-key? key (cdr table))))
	(if record
	    (cdr record)
	    #f)))
    (define (insert! key value)
      (let ((record (assoc same-key? key (cdr table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! table
		      (cons (cons key value) (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE!" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put (list 'a 'a) 1)
(put (list 'a 'a) 2)
(put (list 'a 'b) 3)
(put (list 'a 'b) 4)
(put (list 'a 'b) "test")
(display (get (list 'a 'a))) (newline)
(display (get (list 'a 'b))) (newline)
