;; 実際にm1,m2をaに置換してみると下記のようになる
(define (multiplier a a b)
  (define (process-new-value)
    (cond ((or (and (has-value? a) (= (get-value a) 0))
               (and (has-value? a) (= (get-value a) 0)))
           (set-value! b 0 me))
          ((and (has-value? a) (has-value? a))
           (set-value! b
                       (* (get-value a) (get-value a))
                       me))
          ;; 例えば、b=4,a=3だった場合。set-value!でa=3/4がセットされる。
          ;; だから、Louisの案はダメだということかな？
          ((and (has-value? b) (has-value? a))
           (set-value! a
                       (/ (get-value b) (get-value a))
                       me))
          ((and (has-value? b) (has-value? a))
           (set-value! a
                       (/ (get-value b) (get-value a))
                       me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect a me)
  (connect a me)
  (connect b me)
  me)
