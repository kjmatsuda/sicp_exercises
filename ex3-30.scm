;;;; 自分の回答
(define (ripple-carry-adder c a-list b-list s-list)
  (let ((c (make-wire)))
    (if (not (null? (car a-list)))
        (begin
          (full-adder c (car a-list) (car b-list) (car s-list))
          (ripple-carry-adder c (cdr a-list) (cdr b-list) (cdr s-list))))))

;;;; ネットの回答見たあとの振り返り 
;; 自分の回答は回答例2に近いものだった

;;;; ネットの回答
;; 回答例1
;; http://community.schemewiki.org/?sicp-ex-3.30
(define (ripple-carry-adder Ak Bk Sk C) 
  (define (iter A B S c-in c-out) 
    (if (null? A) 
        S 
        (begin (full-adder (car A) (car B) 
                           c-in (car S) c-out) 
               (iter (cdr A) (cdr B) (cdr S) 
                     (c-out) (make-wire))))) 
  (iter Ak Bk Sk C (make-wire)))

;; 回答例2
;; http://community.schemewiki.org/?sicp-ex-3.30
(define (ripple-carry-adder a b s c) 
  (let ((c-in (make-wire))) 
    (if (null? (cdr a)) 
        (set-signal! c-in 0) 
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))
