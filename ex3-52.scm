;; (1)テキスト中の各式が評価された後のsumの値を答えよ
;; (2)stream-refとdisplay-streamの出力が何か答えよ
;; (3)これらの結果はもし(delay <exp>)を(lambda () <exp>)と定義していたら、異なるものになるか答えよ

;;;;;;  (1)の答えと(2)の答え
;; ひとまず(define z ~~)まではsumは0のまま

;; (stream-ref (stream-filter even?
;;                            (stream-map accum
;;                                        (stream-enumerate-interval 1 20))) 7)

;; 1から20までのうち、偶数は2,4,6,8,10,12,14,16,18,20
;; 7番目は14。->(2)の答え
;; stream-mapは1から20までのうち、1から14までを足し合わせる。
;; よって(stream-ref y 7)後は105になる。

;; 次に(display-stream z)。

;; (display-stream (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                                (stream-map accum (stream-enumerate-interval 1 20))))

;; 出力はフィルタリングされた後の以下の値になる。->(2)の答え
;; 5
;; 10
;; 15
;; 20

;; accumについては1から20のすべてが105にさらに足される。よって315。

;;;;;; (3)の答え
;; 出力結果は変わらない

(define )



