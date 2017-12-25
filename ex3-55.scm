(define (partial-sums s)
  (cons-stream (car-stream s) (add-stream s
                                          (partial-sums (cdr-stream s)))))
