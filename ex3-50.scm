(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) ;; 1.
      the-empty-stream
      (else ;; 2. -> 間違いだった。正しくは cons-stream
       (apply proc (map stream-car argstreams)) ;; 3. 
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) ;; 4.
