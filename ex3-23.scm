(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (make-item value)
  (cons value (cons '() '())))

(define (set-next-item! item next)
  (set-cdr! (cdr item) next))

(define (next-item item)
  (cddr item))

(define (prev-item item)
  (cadr item))

(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))

(define (value-of-item item)
  (car item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "empty queue")
      ((value-of-item (front-ptr queue)))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "empty queue")
      ((value-of-item (rear-ptr queue)))))

(define (rear-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-prev-item! new-item (rear-ptr queue))
           (set-next-item! (rear-ptr queue) new-item)
           (set-rear-ptr! queue new-item)
           queue))))

(define (front-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-next-item! new-item (front-ptr queue))
           (set-front-ptr! queue new-item)
           queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty queue"))
        (else
         (set-front-ptr! queue (next-item (front-ptr queue)))
         queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty queue"))
        (else
         (set-rear-ptr! queue (prev-item (rear-ptr queue)))
         queue)))

(define (display-queue queue)
  (define (display-queue-internal q)
    (cond ((eq? q (rear-ptr queue))
           (display " ")
           (display (value-of-item q)))
          (else
           (begin (display " ")
                  (display (value-of-item q))
                  (display-queue-internal (next-item q))))))
  (if (empty-queue? queue)
      (display "empty queue\n")
      (begin
        (display "(")
        (display-queue-internal (front-ptr queue))
        (display ")\n"))))