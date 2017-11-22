
(define (make-accumulator x)
  (lambda (add-num)
    (set! x (+ x add-num))))
