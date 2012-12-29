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
       (accumulate op init (map car seqs))
       ;; cons と cdr を組み合わせたものを seqs に適用したい
       (accumulate-n op init (map cdr seqs)))))

;; Ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
	 (dot-product v w))
       m))
;; 実行結果
;; (matrix-*-vector (list (list 1 2) (list 3 4)) (list 5 6))
;;  => (17 39)

(define (transpose mat)
  (accumulate-n cons '() mat))
;; 実行結果
;; (transpose (list (list 1 2 3) (list 4 5 6 )))
;; ((1 4) (2 5) (3 6))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) 
	   (matrix-*-vector cols v))
	 m)))
;; 実行結果
;; (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
;; ((19 22) (43 50))

;; Ex 2.46
(define (make-vect xcord ycord)
  (cons xcord ycord))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v)
		(xcor-vect w))
	     (+ (ycor-vect v)
		(ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v)
		(xcor-vect w))
	     (- (ycor-vect v)
		(ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

;; Ex 2.47
;; まず1つ目の make-frame に対して
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define v0 (make-vect 1 2))
(define v1 (make-vect 2 4))
(define v2 (make-vect 4 8))
(define f (make-frame v0 v1 v2))

;; 次に2つ目の make-frame に対して
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge1-frame frame)
  (car (car (cdr frame))))

(define (edge2-frame frame)
  (car (cdr (car (cdr frame)))))

;; Ex 2.48
(define (make-segment start-seg end-seg)
  (cons start-seg end-seg))

(define (start-segment line-seg)
  (car line-seg))

(define (end-segment line-seg)
  (cdr line-seg))

;; Ex 2.49
(define outline
  (segments->painter (list
		      (make-segment (make-vect 0 0)
				    (make-vect 0 1))
		      (make-segment (make-vect 0 1)
				    (make-vect 1 1))
		      (make-segment (make-vect 1 1)
				    (make-vect 1 0))
		      (make-segment (make-vect 1 0)
				    (make-vect 0 0)))))

(define cross
  (segments->painter (list
		      (make-segment (make-vect 0 0)
				    (make-vect 1 1))
		      (make-segment (make-vect 0 1)
				    (make-vect 1 0)))))

(define diamond
  (segments->painter (list
		      (make-segment (make-vect 0 0.5)
				    (make-vect 0.5 1))
		      (make-segment (make-vect 0.5 1)
				    (make-vect 1 0.5))
		      (make-segment (make-vect 1 0.5)
				    (make-vect 0.5 0))
		      (make-segment (make-vect 0.5 0)
				    (make-vect 0 0.5)))))
;; Ex 2.50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

;; Ex 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((point-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (point-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(point-bottom frame)
	(point-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
		    (rotate270 painter2))))

;; Ex 2.54
(define (my-equal? items1 items2)
  (cond ((and (not (pair? items1))
	      (not (pair? items2)))
	 (eq? items1 items2))
	((and (pair? items1)
	      (pair? items2))
	 (and (equal? (car items1) (car items2))
	      (equal? (cdr items1) (cdr items2))))
	(else
	 #f)))

;; Ex 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base power)
  (cond ((=number? base 0) 0)
	((=number? power 0) 1)
	((=number? power 1) base)
	(else (list '** base power))))

(define (base exp)
  (cadr exp))

(define (power exp)
  (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	;; 次の節を指数用に追加する
	((exponentiation? exp)
         (make-product (make-product (power exp)
				     (make-exponentiation (base exp)
							  (- (power exp) 1)))
		       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Ex 2.59
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1)
	       (union-set (cdr set1) set2)))
	(else
	 (union-set (cdr set1) set2))))

;; Ex 2.61
(define (adjoin-set x set)
  (cond ((null? set)
	 (cons x '()))
	((= x (car set))
	 set)
	((< x (car set))
	 (cons x set))
	(else
	 (cons (car set) (adjoin-set x (cdr set))))))

;; Ex 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((> x1 x2)
		  (cons x2 (union-set set1 (cdr set2)))))))))

;; Ex 2.63
;; 木構造を定義
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; Ex 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (entry set-of-records)) #t)
	((< given-key (entry set-of-records))
	 (lookup given-key (left-branch set-of-records)))
	(else ;意図としては (> given-key (entry set-of-records))
	 (lookup given-key (right-branch set-of-records)))))

;; ハフマン符号化
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)   ; symbol
			       (cadr pair))  ; frequency
		    (make-leaf-set (cdr pairs))))))
;; Ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((or (null? symbol) 
	     (not (member symbol (symbols tree))))
	 (error "symbol is not in tree -- ENCODE-SYMBOL" symbol))
	((or (null? tree) (leaf? tree))
	 '()
	 )
	((member symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((member symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))))

(define sample-message-alphabet '(A D A B B C A))

;; Ex 2.69
(define (generate-haffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (define (successive-merge-1 set result)
    (cond ((null? set) '())
	  ((null? (cdr set)) result)
	  (else
	   (successive-merge-1 (adjoin-set (make-code-tree (car set) (cadr set))
				(cddr set))
			       (adjoin-set (make-code-tree (car set) (cadr set))
				result)))))
  (successive-merge-1 set '()))

(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

;; Ex 2.70
(define symbols-of-rock-songs 
  '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))

(define rock-lyrics
  '(GET A JOB
	SHA NA NA NA NA NA NA NA NA
	GET A JOB
	SHA NA NA NA NA NA NA NA NA
	WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
	SHA BOOM))
(encode 
 rock-lyrics
 (generate-haffman-tree symbols-of-rock-songs))

;; 2.4.1 Representations for Complex Numbers
;; テキストから引用
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;; Ben の方法
;; 複素数を直交座標系で表現
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define z1 (make-from-real-imag 3 4))
(define z2 (make-from-real-imag 2 9))

;; Alyssa の方法
;; 複素数を極座標系で表現
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))

;; Ben, Alyssa の方法を共存させるための方法
;; 方法1：タグを用いて分岐させる
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Ben の方法(直交座標)
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y) 
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular 
	      (cons (* r (cos a)) (* r (sin a)))))

(define z1 (make-from-real-imag-rectangular 5 8))
(define z2 (make-from-real-imag-rectangular 7 4))

;; Alyssa の方法(極座標)
(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
	      (cons r a)))

;; generic selector
(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type -- ANGLE" z))))

;; constructor
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; Ben, Alyssa の方法を共存させるための方法
;; 方法2：表を使った方法

;; get put の実装
;; 引用元  http://d.hatena.ne.jp/higepon/20060503/1146317558
(define op-table (make-hash-table))

(define (put op type item)
    (if (not (hash-table-exists? op-table op))
        (hash-table-put! op-table op (make-hash-table)))
    (let ((type-table (hash-table-get op-table op)))
      (hash-table-put! type-table type item)))

(define (get op type)
    (if (not (hash-table-exists? op-table op))
        (hash-table-put! op-table op (make-hash-table)))
    (let ((type-table (hash-table-get op-table op)))
      (hash-table-get type-table type)))

;; Ben の方法(直交座標)
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Alyssa の方法(極座標)
(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error 
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Ex 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; 2.5.1 Generic Arithmetic Operations のソースコードを引用
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; ordinary numbers に対するパッケージ
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 次に有理数に対するパッケージ
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  (get 'make 'rational) n d)
  
;; 複素数に対するパッケージ
(define (install-complex-package)
  ;; imported prodedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Ex 2.77
(define z1
  (make-complex-from-real-imag 3 4))

;; 各関数をトレースする
(use slib)
(require 'trace)

(trace magnitude)
(trace get)
(trace apply-generic)
(trace type-tag)

なぜか以下のエラーが出てしまう
gosh> (magnitude z1)
           CALL magnitude (complex rectangular 3 . 4)
            CALL apply-generic magnitude (complex rectangular 3 . 4)
             CALL type-tag (complex rectangular 3 . 4)
             RETN type-tag complex
*** ERROR: #<hash-table eq? 0x913c730> doesn't have an entry for key (complex)
