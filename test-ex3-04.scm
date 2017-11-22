(add-load-path ".")
(load "ex3-04.scm")
(use gauche.test)
(test-start "ex3-4")
(test-section "make-account")
(define acc (make-account 100 'secret-password))
(test-section "valid-password")
(test* "valid"
 ((acc 'secret-password 'withdraw) 40)
 60)
(test* "valid"
 ((acc 'secret-password 'deposit) 50)
 110)
(test* "too much withdraw"
 ((acc 'secret-password 'withdraw) 111)
 "Insufficient funds")
(test-section "7回以上連続で不正なパスワード")
(test* "withdraw"
 ((acc 'some-other-password 'withdraw) 50)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Call the cops!")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Call the cops!")
(test-section "3回連続不正なパスワード、1回正しいパスワード、4回不正")
(test* "valid"
 ((acc 'secret-password 'deposit) 40)
 150)
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "valid"
 ((acc 'secret-password 'deposit) 10)
 160)
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
