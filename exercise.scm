;; Ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Ex 1.8
;; なぜか処理が終了しない。 2012/9/17(月)
(define (good-enough? guess x)
  (<= (abs (- guess x)) 0.001))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
		      x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
	(* guess 2.0))
     3.0))

(define (cube-root x)
  (cube-root-iter 1.0 x))

