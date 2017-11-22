(add-load-path ".")
(load "ex3-02.scm")
(use gauche.test)
(test-start "ex3-2")
(test-section "make-monitored")
(define monitored-fn (make-monitored sqrt))
(test-section "exec make-monitored")
(test* "sqrt"
 (monitored-fn 9)
 3)
(test* "sqrt"
 (monitored-fn 16)
 4)
(test* "sqrt"
 (monitored-fn 25)
 5)
(test* "sqrt"
 (monitored-fn 100)
 10)
(test-section "how-many-calls, and reset-count")
(test* "how-many-calls"
 (monitored-fn 'how-many-calls)
 4)

(monitored-fn 100)

(test* "how-many-calls"
 (monitored-fn 'how-many-calls)
 5)

(test* "reset-count"
 (monitored-fn 'reset-count)
 0)

(test* "how-many-calls"
 (monitored-fn 'how-many-calls)
 0)

(monitored-fn 100)

(test* "how-many-calls"
 (monitored-fn 'how-many-calls)
 1)
