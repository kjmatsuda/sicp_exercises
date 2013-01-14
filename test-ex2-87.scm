(add-load-path ".")
(load "./ex2-87.scm")
(use gauche.test)

(test-start "ex2-87")
(test-section "=zero? for polynomial (coefficients are integer)")
(define poly-x-zero-1
  (make-polynomial 'x '((2 0) (1 0) (0 0))))
(define poly-x2
  (make-polynomial 'x '((2 1) (1 0) (0 0))))
(define poly-x3
  (make-polynomial 'x '((2 0) (1 1) (0 0))))
(define poly-x4
  (make-polynomial 'x '((2 0) (1 0) (0 1))))
(test* "=zero?"
       (=zero? poly-x-zero-1)
       #t)
(test* "=zero?"
       (=zero? poly-x2)
       #f)
(test* "=zero?"
       (=zero? poly-x3)
       #f)
(test* "=zero?"
       (=zero? poly-x4)
       #f)
(test-end)
