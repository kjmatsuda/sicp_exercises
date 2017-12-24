(define (mul-sreams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-sreams factorials integers)))
