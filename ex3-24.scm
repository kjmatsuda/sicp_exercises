(define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

;; http://d.hatena.ne.jp/bowmoq/20090605/1244156961
;; 「いらずんば」から引用
(define (make-table same-key?)
  (define (my-assoc key records) ;; 比較をする述語を指定する
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (my-assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (my-assoc key-2 (cdr subtable))))
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