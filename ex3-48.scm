(define (make-account-and-serializer balance acc-number)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'acc-number) acc-number)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (acc-number1 (account1 'acc-number))
        (serializer2 (account2 'serializer))
        (acc-number2 (account2 'acc-number))
        )
    (if (<= acc-number1 acc-number2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))
