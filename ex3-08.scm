;; (+ (f 0) (f 1)) で 
;; (f 0)が先に評価される場合は 0
;; (f 1)が先に評価される場合は 1を返すような関数 f を定義する
(define (f x)
  (let ((multiplier x))
    (define (g y)
      (print "multiplier=" multiplier)
      (print "y=" y)
      (print "x=" x)
      (* multiplier y)
      (set! multiplier y))
    (g x)))
;; あれ~できないなあ。グローバル変数を使うのかな？