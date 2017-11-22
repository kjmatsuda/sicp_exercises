(add-load-path ".")
(load "ex3-01.scm")
(use gauche.test)
(test-start "ex3-1")
(test-section "make-accumulator")
(define first-accumulator (make-accumulator 10))
(define second-accumulator (make-accumulator 100))
(test-section "first-accumulator")

(test* "first-accumulator"
 (first-accumulator 2)
 12)
(test* "first-accumulator"
 (first-accumulator 3)
 15)
(test* "first-accumulator"
 (first-accumulator 4)
 19)
(test-section "second-accumulator")
(test* "second-accumulator"
 (second-accumulator 3)
 103)
(test* "second-accumulator"
 (second-accumulator 40)
 143)
(test-section "first-accumulator again")
(test* "first-accumulator"
 (first-accumulator 1)
 20)



