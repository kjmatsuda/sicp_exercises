(define (make-account balance secret-password)
  ;; 連続でパスワードを間違えたら警察に通報
  (define limit-mistake 7)
  ;; 連続でパスワードを間違えた回数
  (define consecutive-mistake 0)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (error-message amount)
    ;; 引数に金額を受け取るが無視
    "Incorrect Password")
  (define (call-the-cops amount)
    ;; 引数に金額を受け取るが無視
    "Call the cops!")
  (define (dispatch password fn)
    (cond ((or (null? password) (not (eq? password secret-password)))
	   (set! consecutive-mistake (+ 1 consecutive-mistake))
	   (if (>= consecutive-mistake limit-mistake)
	       call-the-cops
	       error-message))
	  ((eq? fn 'withdraw) 
	   (set! consecutive-mistake 0)
	   withdraw)
	  ((eq? fn 'deposit)
	   (set! consecutive-mistake 0)
	   deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)
