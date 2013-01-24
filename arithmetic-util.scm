(use srfi-27)

(define (square x)
  (* x x))

(define (random n)
  ;; デフォルトの乱数発生源を用いて、0 から n-1 までの正確な整数の乱数を返す
  (random-integer n))
