(add-load-path ".")
(load "ex3-3.scm")
(use gauche.test)
(test-start "ex3-3")
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
(test-section "invalid-password")
(test* "withdraw"
 ((acc 'some-other-password 'withdraw) 50)
 "Incorrect Password")
(test* "deposit"
 ((acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
