(add-load-path ".")
(load "ex2-79.scm")
(use gauche.test)

(test-start "ex2-79")
(test-section "ordinary number")
(define n1
  (make-scheme-number 7))
(define n2
  (make-scheme-number 7))
(define n3
  (make-scheme-number 8))
(test* "equal"
       #t
       (equ? n1 n2))
(test* "equal"
       #f
       (equ? n1 n3))

;; 有理数
(test-section "rational number")
(define r1
  (make-rational 2 3))
(define r2
  (make-rational 2 3))
(define r3
  (make-rational 4 6))
(define r4
  (make-rational 2 5))
(define r5
  (make-rational 1 3))
(define r6
  (make-rational 4 5))
(test* "equal"
       #t
       (equ? r1 r2))
(test* "equal"
       #t
       (equ? r1 r3))
(test* "equal"
       #f
       (equ? r1 r4))
(test* "equal"
       #f
       (equ? r1 r5))
(test* "equal"
       #f
       (equ? r1 r6))

(test-section "複素数(直交座標)")
(define z1-real-imag
  (make-complex-from-real-imag 3 4))
(define z2-real-imag
  (make-complex-from-real-imag 3 4))
(define z3-real-imag
  (make-complex-from-real-imag 4 4))
(define z4-real-imag
  (make-complex-from-real-imag 3 5))
(test* "equal"
       #t
       (equ? z1-real-imag z2-real-imag))
(test* "equal"
       #f
       (equ? z1-real-imag z3-real-imag))
(test* "equal"
       #f
       (equ? z1-real-imag z4-real-imag))
(test* "equal"
       #f
       (equ? z3-real-imag z4-real-imag))

(test-section "複素数(極座標)")
(define z1-mag-ang
  (make-complex-from-mag-ang 5 3.14))
(define z2-mag-ang
  (make-complex-from-mag-ang 5 3.14))
(define z3-mag-ang
  (make-complex-from-mag-ang 4 3.14))
(define z4-mag-ang
  (make-complex-from-mag-ang 5 1.57))
(test* "equal"
       #t
       (equ? z1-mag-ang z2-mag-ang))
(test* "equal"
       #f
       (equ? z1-mag-ang z3-mag-ang))
(test* "equal"
       #f
       (equ? z1-mag-ang z4-mag-ang))
(test* "equal"
       #f
       (equ? z3-mag-ang z4-mag-ang))
(test-end)
