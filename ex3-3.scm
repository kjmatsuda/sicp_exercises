(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; dispatch でパスワードをチェックして文字列を返すと
  ;; テストでエラーが出る(返り値に関数を期待しているため)。
  ;; パスワードのチェックはそれぞれの関数でやるべきなんだろうか。
  ;; コードの重複が気になる
  ;; そっか、エラー用文字列を返す関数を定義してやればいいのか。
  (define (error-message amount)
    ;; 引数に金額を受け取るが無視
    "Incorrect Password")
  (define (dispatch password fn)
    (cond ((or (null? password) (not (eq? password secret-password)))
	   error-message)
	  ((eq? fn 'withdraw) withdraw)
	  ((eq? fn 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)
