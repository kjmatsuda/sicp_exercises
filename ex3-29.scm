;; こんな感じかなあと思うんだけど、問題のリクエストのdelay-timeをand-gate-delayと
;; inverter-delayを使って表わせっていうリクエストに答えてない気がする...
;; それは別にソースとして、表さなくてもいいのかな
;; delay-time は inverter-delay + and-gate-delay + inverter-delayかな？
(define (or-gate o1 o2 output)
  (let ((a1 (make-wire))
        (a2 (make-wire))
        (i1 (make-wire)))
    (inverter o1 a1)
    (inverter o2 a2)
    (and-gate a1 a2 i1)
    (inverter i1 output)
    'ok))
