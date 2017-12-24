(load "./table-util-local.scm")
(use gauche.test)
(test-start "table-util-local")

(define table1 (make-table))
(test-section "insert, and lookup")
(test* "lookup, before insert)"
       ((table1 'lookup-proc) 'animal 'cat)
       #f)
((table1 'insert-proc) 'animal 'cat 'myaa)
(test* "lookup, after insert"
       ((table1 'lookup-proc) 'animal 'cat)
       'myaa)
((table1 'insert-proc) 'animal 'cat 'cute)
(test* "lookup, after update"
       ((table1 'lookup-proc) 'animal 'cat)
       'cute)
(test* "lookup, before insert"
       ((table1 'lookup-proc) 'animal 'dog)
       #f)
((table1 'insert-proc) 'animal 'dog 'wow)
(test* "lookup, after insert"
       ((table1 'lookup-proc) 'animal 'dog)
       'wow)
((table1 'insert-proc) 'animal 'dog 'large)
(test* "lookup, after update"
       ((table1 'lookup-proc) 'animal 'dog)
       'large)
(test-end)