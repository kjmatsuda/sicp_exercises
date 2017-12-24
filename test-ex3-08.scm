(add-load-path ".")
(load "ex3-08.scm")
(use gauche.test)
(test-start "ex3-8")
(test-section "(f 0)を先に評価")
(test* "(+ (f 0) (f 1))"
       (+ (f 0) (f 1)) 0)
(test-section "(f 1)を先に評価")
(test* "(+ (f 1) (f 0))"
       (+ (f 1) (f 0)) 1)
(test-end)
