(define (assoc key records same-key?)
  (define equal-test '())
  ;; same-key? が null のときは equal? を使うみたいな感じかな
  (if (null? same-key?)
      (set! equal-test equal?)
      (set! equal-test same-key?))
  (cond ((null? records) #f)
	((equal-test key (caar records)) (car records))
	(else (assoc key (cdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable) same-key?)))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable) same-key?)))
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
	    ((eq? m 'insert-proc) insert!)
	    (else (error "Unknown operation -- TABLE" m))))

    dispatch))