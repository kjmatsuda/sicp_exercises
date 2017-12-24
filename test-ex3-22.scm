(load "./ex3-22.scm")
(use gauche.test)
(test-start "ex3-22")
(define q1 (make-queue))
(test-section "queue with local env")
((q1 'insert-queue!) 'a)
(test* "insert"
       ((q1 'print-queue))
       '(a))
((q1 'insert-queue!) 'b)
(test* "insert"
       ((q1 'print-queue))
       '(a b))
((q1 'insert-queue!) 'c)
(test* "insert"
       ((q1 'print-queue))
       '(a b c))
((q1 'delete-queue!))
(test* "delete"
       ((q1 'print-queue))
       '(b c))
((q1 'delete-queue!))
(test* "delete"
       ((q1 'print-queue))
       '(c))
((q1 'delete-queue!))
(test* "delete"
       ((q1 'print-queue))
       '())
(test-end)