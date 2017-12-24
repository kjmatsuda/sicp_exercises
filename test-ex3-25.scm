(load "./ex3-25.scm")
(use gauche.test)
(test-start "ex3-25")

(define table1 (make-table))
(define lookup (table1 'lookup-proc))
(define insert! (table1 'insert-proc!))
(define print-table (table1 'print-table))
(insert! '(japan tokyo 2009/2/13) 20.1)
(insert! '(japan osaka 2009/2/13) 22.0)
(insert! '(usa newyork 2009/2/13) 14.5)

(test-section "lookup")
(test* "(lookup '(japan tokyo 2009/2/13))"
       (lookup '(japan tokyo 2009/2/13))
       20.1)
(test-section "lookup")
(test* "(lookup '(japan osaka 2009/2/13))"
       (lookup '(japan osaka 2009/2/13))
       22.0)
(test-section "lookup")
(test* "(lookup '(usa newyork 2009/2/13))"
       (lookup '(usa newyork 2009/2/13))
       14.5)
(test-section "lookup")
(test* "(lookup '(usa tokyo 2009/2/13))"
       (lookup '(usa tokyo 2009/2/13))
       #f)
(test-end)