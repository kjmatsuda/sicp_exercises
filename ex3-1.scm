(define (make-accumulator sum)
  (lambda (addend)
    (set! sum (+ sum addend))
    sum))