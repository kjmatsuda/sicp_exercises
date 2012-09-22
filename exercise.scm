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

;; Ex 1.22

;;gauche用runtime(現在時刻)関数
;;sys-gettimeofdayは(values 秒数 マイクロ秒)を返す
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-devisor)
  (cond ((> (* test-devisor test-devisor) n) n)
	((divides? test-devisor n) test-devisor)
	(else (find-divisor n (+ test-devisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; 連続した奇数から素数を見つける関数
;; 特定の範囲: start から end まで
;; 問題(Ex 1.22)中で最小の素数3つを取得するのにかかる時間を測定するので
;; goal で制御する
(define (search-for-primes start end found-prime goal)
  (if (and (< start end) (< found-prime goal))
      (if (prime? start)
	  ;; 素数が見つかった
	  (search-for-primes (+ start 2) end (+ found-prime 1) goal)
	  (if (= (remainder start 2) 0)
	      ;; 偶数だったとき
	      (search-for-primes (+ start 1) end found-prime goal)
	      ;; 奇数だったとき
	      (search-for-primes (+ start 2) end found-prime goal)))))
