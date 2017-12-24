(load "./ex3-21.scm")
(use gauche.test)
(test-start "ex3-21")
(define q1 (make-queue))
(test-section "print-queue")
(test* "insert"
       (print-queue (insert-queue! q1 'a))
       '(a))
(test* "insert"
       (print-queue (insert-queue! q1 'b))
       '(a b))
(test* "insert"
       (print-queue (insert-queue! q1 'c))
       '(a b c))
(test* "delete"
       (print-queue (delete-queue! q1))
       '(b c))
(test* "delete"
       (print-queue (delete-queue! q1))
       '(c))
(test* "delete"
       (print-queue (delete-queue! q1))
       '())
(test-end)