(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            ;; bがマイナスの場合
            (error "square less than 0 -- SQUARER" (get-value b))
            ;; bがプラスの場合
            (set-value! a (ルート (get-value b))))
        ;; bに値がない場合
        (if (has-value? a)
            ;; aに値がある場合
            (set-value! b (* (get-value a) (get-value a))))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
