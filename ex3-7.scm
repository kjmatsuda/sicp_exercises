;; make-account の中にmake-jointを追加するというわけではなさそうだな
(define (make-joint acc password new-password)
  ;; パスワードの追加
  ((acc password 'add-password) new-password) acc)
  
(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  ;; 返り値はパスワードが追加されたというメッセージ
  (define (add-password new-password)
    (set! secret-password (cons new-password secret-password))
    (format "Add password: ~a" new-password))
  
  (define (error-message amount)
    ;; 引数に金額を受け取るが無視
    "Incorrect Password")
  (define (dispatch password fn)
    ;; パスワード照合の変更
    (cond ((or (null? password) (not (memq password secret-password)))
	   error-message)
	  ((eq? fn 'withdraw) withdraw)
	  ((eq? fn 'deposit) deposit)
	  ((eq? fn 'add-password) add-password)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  ;; パスワードをmemqで比較したいがために set!しているが、とてもダサい
  (set! secret-password (cons secret-password '()))
  dispatch)
;; TODO 今の解答では
;; (define paul-acc
;;   (make-joint peter-acc 'open-sesame 'rosebud))
;; で paul-acc, peter-acc の両方から 'open-sesame 'rosebud を使ったアクセスが
;; 可能になってしまう。
;; おそらく本来は
;; peter-acc → open-sesame
;; paul-acc → rosebud
;; のみを許容するべき

