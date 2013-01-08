(define (square x)
  (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define package-table (make-hash-table 'equal?))

;手続きを入れ込む手続き
(define (put operation-symbol type-symbol operation)
  (let ((operations-table (hash-table-get package-table type-symbol #f)))
    (if (equal? #f operations-table)
	;typeが存在しなかった場合は作成しちゃう。
	(let ((tmp (make-hash-table)))
	  (hash-table-put! tmp
			   operation-symbol
			   operation)
	  (hash-table-put! package-table
			   type-symbol
			   tmp))
	;存在した場合はそのテーブルに対して該当する key-value エントリを生成 or 更新
	(hash-table-put! operations-table
			 operation-symbol
			 operation))))

;手続きを取り出す手続き
(define (get operation-symbol type-symbol)
  (let ((operations-table (hash-table-get package-table type-symbol #f)))
    (if (equal? #f operations-table)
	#f
	(hash-table-get operations-table
			operation-symbol
			#f))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error 
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; 2.5.1 Generic Arithmetic Operations のソースコードを引用
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? x y)
  (apply-generic 'equ? x y))
