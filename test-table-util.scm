(load "./table-util.scm")
(use gauche.test)
(test-start "table-util")

(define table1 (make-table))
(test-section "insert, and lookup")
(test* "lookup, before insert)"
       (lookup 'animal 'cat table1)
       #f)
(insert! 'animal 'cat 'myaa table1)
(test* "lookup, after insert"
       (lookup 'animal 'cat table1)
       'myaa)
(insert! 'animal 'cat 'cute table1)
(test* "lookup, after update"
       (lookup 'animal 'cat table1)
       'cute)
(test* "lookup, before insert"
       (lookup 'animal 'dog table1)
       #f)
(insert! 'animal 'dog 'wow table1)
(test* "lookup, after insert"
       (lookup 'animal 'dog table1)
       'wow)
(insert! 'animal 'dog 'large table1)
(test* "lookup, after update"
       (lookup 'animal 'dog table1)
       'large)
(test-end)