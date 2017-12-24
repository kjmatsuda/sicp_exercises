(add-load-path ".")
(load "ex3-07.scm")
(use gauche.test)
(test-start "ex3-7")
(test-section "make-account")
(define peter-acc (make-account 100 'open-sesame))
(test-section "valid-password")
(test* "valid"
 ((peter-acc 'open-sesame 'withdraw) 40)
 60)
(test* "valid"
 ((peter-acc 'open-sesame 'deposit) 50)
 110)
(test* "too much withdraw"
 ((peter-acc 'open-sesame 'withdraw) 111)
 "Insufficient funds")
(test-section "invalid-password")
(test* "withdraw"
 ((peter-acc 'some-other-password 'withdraw) 50)
 "Incorrect Password")
(test* "deposit"
 ((peter-acc 'some-other-password 'deposit) 40)
 "Incorrect Password")
(test-section "make-joint")
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
(test* "deposit"
 ((paul-acc 'rosebud 'deposit) 40)
 150)
(test* "deposit"
 ((paul-acc 'open-sesame 'deposit) 40)
 "Incorrect Password")
(test* "deposit"
 ((peter-acc 'open-sesame 'deposit) 50)
 200)
(test* "deposit"
 ((peter-acc 'rosebud 'deposit) 50)
 "Incorrect Password")
(test-end)
