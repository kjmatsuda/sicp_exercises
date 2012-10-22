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
  (cond ((>= found-prime goal)
	 (display "search finished. We reach goal!"))
	((>=  start end)
	 (display "search finished."))
	((prime? start)
	 ;; 素数が見つかった
	 ;; (display start)
	 ;; (newline)
	 (search-for-primes (+ start 2) end (+ found-prime 1) goal))
	(else
	 (if (= (remainder start 2) 0)
	      ;; 偶数だったとき
	      (search-for-primes (+ start 1) end found-prime goal)
	      ;; 奇数だったとき
	      (search-for-primes (+ start 2) end found-prime goal)))))

(define (timed-search-for-primes-test n)
  (display "search start from ")
  (display n)
  (newline)
  (start-search-for-primes-test n (runtime)))

(define (start-search-for-primes-test n start-time)
  ;; when three primes are found, search is finished.
  (search-for-primes n (* 10 n) 0 3)
  (report-prime (- (runtime) start-time)))

;; Ex 1.23
(define (find-divisor n test-devisor)
  (cond ((> (* test-devisor test-devisor) n) n)
	((divides? test-devisor n) test-devisor)
	(else (find-divisor n (next test-devisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; Ex 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a (term a))) ;; 間違い。正しくは (iter a 0)

;; Ex 1.31
;; answer for a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (define (fact-next x) (+ x 1))
  (product identity 1 fact-next n))

;; approximation for pi
(define (square-product a b)
  (define (square x)
    (* x x))
  (define (next x)
    (+ 2 x))
  (product square a next b))

(define (approximate-pi n)
  (/ (/ (square-product 4 n)
	(square-product 3 n))
     2.0))

;; answer for b
;; it generates iterative process
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

;; Ex 1.32
;; answer for a
;; write more abstruct procedure than sum and product
;; this generates recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))
;; use accumulate above, write easier version of sum and product
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; iterative vesion of accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
	result
	(iter combiner (next a) (combiner a result))))
  (iter combiner a null-value))

;; Ex 1.34
(define (f g)
  (g 2))

;; Ex 1.36
;; modify fixed-point so that it prints the sequence of approximations
(define tolerance 0.00001)
(define (fixed-point-print-process f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "current value:")
      (display guess)
      (newline)
      (display "next value:")
      (display next)
      (newline)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (search-for-answer-of-lambda)
  (fixed-point-print-process (lambda (y) (/ (log 1000)
					    (log y)))
			     1.1))
;; Ex 1.41
(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))
(define (double func)
  (lambda (x) (func (func x))))

;; Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
;; 確認
;; ((compose square inc) 6)
;; 49

;; Ex 1.43
(define (repeated f n)
  (if (<= n 1)
      f
      (repeated (compose f f) (- n 1))))
;; 確認
;; ((repeated square 2) 5)
;; 625

;; Ex 2.1
(define (make-rat n d)
  (let ((signed-n (* (normalize-sign n d) n))
	(signed-d (* (normalize-sign n d) d)))
    (let ((g (gcd signed-n signed-d)))
      (cons (/ n g) (/ d g)))))

(define (normalize-sign n d)
  (if (or (and (< n 0) (< d 0))
	  (and (> n 0) (< d 0)))
      -1
      1))

;; Ex 2.2
;; definition of line segment
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; definition of point
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; returns midpoint of line segment
(define (midpoint-segment segment)
  (cons (average (x-point (start-segment segment))
		 (x-point (end-segment segment)))
	(average (y-point (start-segment segment))
		 (y-point (end-segment segment)))))


(define (average a b)
  (/ (+ a b) 2.0))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Ex 2.7
(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; Ex 2.12
;; make-center-width を使って表現すればよかった
;; percent が0~100であることを考慮に入れてなかった
(define (make-center-percent c percent)
  (make-interval (- c (* c percent))
		 (+ c (* c percent))))

(define (percent i)
  (/ (center i)
     (/ width 2))) ;; width を2で割る必要はなさそうだ
                   ;; 分母、分子が逆になってる

;; Ex 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; Ex 2.18
(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))
;; (reverse (list 1 4 9 16 25))
;;  => ((((25 . 16) . 9) . 4) . 1) が返ってきて何かおかしい

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))

;; Ex 2.20
(define nil '())
(define (both-even-or-odd x y)
  ;; 奇数同士、もしくは偶数同士であれば #t を返す
  (if (or (and (= (remainder x 2) 1)
	       (= (remainder y 2) 1))
	  (and (= (remainder x 2) 0)
	       (= (remainder y 2) 0)))
      #t
      #f))

(define (same-parity first-elem . option)
  (define (iter-parity rest result)
    ;;;; for debug
    ;; (display "残り：")
    ;; (display rest)
    ;; (newline)
    ;; (display "結果：")
    ;; (display result)
    ;; (newline)
    ;; (newline)    
    (if (null? rest)
	result
	(if (both-even-or-odd first-elem (car rest))
	    (iter-parity (cdr rest) (append result (list (car rest))))
	    (iter-parity (cdr rest) result))))
  (iter-parity option (list first-elem)))

;; Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (square-list-with-map items)
  (map (lambda (x) (* x x))
       items))

;; Ex 2.27
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (cond ((not (pair? items))
	 (list items))
	(else 
	 (append (deep-reverse (cdr items)) 
		 (deep-reverse (car items))))))

;; Ex 2.28
(define (fringe items)
  (cond ((null? (car items))
	 (display "null のとき：")
	 (display items)
	 (newline)
	 items)
	((not (pair? (car items)))
	 (display "ペアでない：")
	 (display items)
	 (newline)
	 items)
	(else 
	 (display "ペアのとき：")
	 (display items)
	 (newline)
	 (append 
	  ;; ここでリストにしてるから常にペアが成立する
	  (fringe (list (car items))) 
	  (fringe (cdr items))))))

;; Ex 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree))
	 (* tree tree))
	(else (cons (square-tree (car tree))
		      (square-tree (cdr tree))))))
;; テストデータ
;; (square-tree 
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; => (1 (4 (9 16) 25) (36 49))

;; map版
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

;; Ex 2.31
(define (tree-map proc tree)
  (cond ((null? tree) nil)
	((not (pair? tree))
	 (proc tree))
	(else (cons (tree-map proc (car tree))
		      (tree-map proc (cdr tree))))))

(define (square-tree-with-map tree)
  (tree-map (lambda (x) (* x x)) tree))

;; Ex 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; sequence が null になるまで展開していく(引数を先に評価するから)
;; sequence が null になったら、リストの後方から値を蓄積する
(define (map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))
;; (append (list 1 2 3 4) (list 5 6 7 8))
;; を実行すると
;; (5 6 7 8 1 2 3 4)
;; になってしまう。後方から cons を始めるため、seq1 の先頭に seq2 を cons した。

;; Ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		;; ペアでなかった場合の条件がいりそうだ
		(* x (+ (* x (car (cdr higher-terms)))
			this-coeff)))
	      0
	      coefficient-sequence))

;; Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons 
       ;; cons と car を組み合わせたものを seqs に適用したい
       (accumulate op init (accumulate cons '() (map car seqs)))
       ;; cons と cdr を組み合わせたものを seqs に適用したい
       (accumulate-n op init (accumulate cons '() (map cdr seqs))))))