(add-load-path ".")
(load "ex2-83.scm")
(use gauche.test)

(test-start "ex2-83")
(test-section "raise from integer to rational")
(define n1
  (make-scheme-number 7))
(define n2
  (make-scheme-number -5))
(test* "raise"
       (make-rational 7 1)
       (raise n1))
(test* "raise"
       (make-rational -5 1)
       (raise n2))
(test-section "raise from rational to real")
(define r1
  (make-rational 2 3))
(define r2
  (make-rational -2 -3))
(define r3
  (make-rational 0 6))
(test* "raise"
       (make-real (/ 2 3))
       (raise r1))
(test* "raise"
       (make-real (/ -2 -3))
       (raise r2))
(test* "raise"
       (make-real (/ 0 6))
       (raise r3))

(test-section "raise from real to complex")
(define real1
  (make-real 3))
(define real2
  (make-real -5.15))
(define real3
  (make-real 0))
(test* "raise"
       (make-complex-from-real-imag 3 0)
       (raise real1))
(test* "raise"
       (make-complex-from-real-imag -5.15 0)
       (raise real2))
(test* "raise"
       (make-complex-from-real-imag 0 0)
       (raise real3))

(test-section "raise from integer(bottom) to complex(top)")
(define i1
  (make-scheme-number 3))
(test* "raise"
       (make-complex-from-real-imag 3 0)
       (raise (raise (raise i1))))
(test-end)
