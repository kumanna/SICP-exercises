(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc same-key? key records)
      (cond ((null? records) #f)
	    ((equal? key (caar records)) (car records))
	    (else (assoc same-key? key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc same-key? key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc same-key? key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 't1 'a 1)
(put 't1 'a 2)
(put 't1 'b 3)
(put 't1 'b 4)
(put 't1 'b "test")
(display (get 't1 'a)) (newline)
(display (get 't1 'b)) (newline)
