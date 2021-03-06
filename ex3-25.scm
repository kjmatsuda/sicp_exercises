;; 引用元 http://www.serendip.ws/archives/1332
(define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (lookup-iter key-list local-table))
    (define (lookup-iter key-list local-table)
      (if (null? key-list)
	  #f
	  (let ((subtable (assoc (car key-list) (cdr local-table))))
	    (if subtable
		(if (null? (cdr key-list))
		    (cdr subtable)
		    (lookup-iter (cdr key-list) subtable))
		#f))))
    (define (insert! key-list value)
      (insert-iter! key-list value local-table))
    (define (insert-iter! key-list value local-table)
      (if (null? key-list)
	  #f
	  (let ((subtable (assoc (car key-list) (cdr local-table))))
	    (if subtable
		(if (null? (cdr key-list))
		    (set-cdr! subtable value)
		    (insert-iter! (cdr key-list) value subtable))
		(set-cdr! local-table
			  (cons (insert-iter key-list value)
				(cdr local-table))))))
      'ok)
    (define (insert-iter key-list value)
      (if (null? (cdr key-list))
	  (cons (car key-list) value)
	  (list (car key-list) (insert-iter (cdr key-list) value))))
    (define (print-table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print-table) print-table)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))