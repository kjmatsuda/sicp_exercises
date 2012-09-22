;; Ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Ex 1.8
(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

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

;; Ex 1.11
;; recursive process
(define (recursive-calc x)
  (if (< x 3)
      x
      (+ (recursive-calc (- x 1))
	 (* 2 (recursive-calc (- x 2)))
	 (* 3 (recursive-calc (- x 3))))))

;; Ex 1.20
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))   ; compute p'
		   (+ (* 2 p q) (* q q)) ; compute q'
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))
	 