(load "./ex3-12.scm")
(use gauche.test)
(test-start "ex3-12")
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(test-section "response after append")
(test* "(cdr x)"
       (cdr x) (list 'b))

(define w (append! x y))
(test-section "response after append!")
(test* "(cdr x)"
       (cdr x) (list 'c 'd))
(test-end)