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
;; append! 内の (set-cdr! (last-pair x) y) で
;; x の cdr 部が y に置き換わると思ったけど
;; 後ろに付け足されるということかな
(test* "(cdr x)"
       (cdr x) (list 'b 'c 'd))
(test-end)