(define (mul-sreams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
               (mul-sreams factorials integers)))

;; 正解は下記
;; 参考サイトからコピペ
(define factorials
  (cons-stream 1
               (mul-sreams
                factorials
                (add-streams ones integers))))

(define factorials
  (cons-stream 1
               (mul-sreams
                factorials
                (stream-cdr integers))))
