(load "./propagation-of-constraints.scm")

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier v c u)
    (constant 2 v)
    'ok))

;; ここからはserendipを参考に実際に動かしてみた結果
(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(averager A B C)

(probe "A" A)

(probe "B" B)

(probe "C" C)

(set-value! A 10 )
