(load "./common.scm")

;; Ex 2.85
(define tower
  '(complex real rational scheme-number))

(define (drop x)
  (if (eq? (last tower) (type-tag x))
      ;; タワーの最下部である
      x
      ;; タワーの最下部ではない
      (let ((proc (get 'project (type-tag x))))
	(print (type-tag x))
	(if proc
	    (let ((proj-x (apply proc (contents x))))
	      (if ((get 'equ? (type-tag x)) proj-x x)
		  ;; 強制的に変換した前後で値が等しい
		  (drop proj-x)
		  ;; 変換した前後で値が異なる
		  x))
	    x))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (higher-type x y)
  (define (iter twr)
    (if (null? twr)
	#f
	(cond ((eq? x (car twr)) x)
	      ((eq? y (car twr)) y)
	      (else (iter (cdr twr))))))
       (iter tower))

(define (coerce-higher-type items)
  (let ((item1 (car items))
        (item2 (cadr items)))
       (let ((type1 (type-tag item1))
             (type2 (type-tag item2)))
            (if (eq? type1 type2)
                items
                (let ((tag (higher-type type1 type2)))
                     (if (eq? tag type1)
                         (coerce-higher-type (list item1 (raise item2)))
                         (coerce-higher-type (list (raise item1) item2))))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags)))
                         (if (eq? type1 type2)
                             (error "E1. No method for these types" (list op type-tags))
                             (let ((coerced-args (coerce-higher-type args)))
                                  (let ((proc (get op (map type-tag coerced-args))))
                                       (if proc
                                           (apply proc (map contents coerced-args))
                                           (error "E2.No method for these types" (list op type-tags)))))))
                    (error "E3. No method for these types" (list op type-tags)))))))

(define (raise x)
  (apply-generic 'raise x))

;; ジェネリック関数の定義を追加
(define (project x)
  (apply-generic 'project x))

;; ordinary numbers に対するパッケージ
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise x)
    (make-rational x 1))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'raise '(scheme-number) raise)
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;; 次に有理数に対するパッケージ
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (equ? x y)
    (= (/ (numer x) (denom x))
       (/ (numer y) (denom y))))
  (define (raise x)
    (make-real (/ (numer x) (denom x))))
  ;; 内部関数に以下を追加
  (define (project x)
    (make-scheme-number (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put 'raise '(rational) raise)
  ;; インタフェースには以下を追加
  (put 'project '(rational) project)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; 実数算術演算パッケージ
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (raise x)
    (make-complex-from-real-imag x 0))
  ;; 内部関数に以下を追加
  (define (project x)
    (make-rational x 1))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real) raise)
  ;; インタフェースには以下を追加
  (put 'project '(real) project)
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(install-real-package)

;; Ben の方法(直交座標)
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular) equ?)
  'done)

;; Alyssa の方法(極座標)
(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
	 (= (angle z1) (angle z2))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar) equ?)
  'done)
  
;; 複素数に対するパッケージ
(define (install-complex-package)
  ;; imported prodedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  ;; 内部関数に以下を追加
  (define (project z)
    (make-real (real-part z)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  ;; インタフェースには以下を追加
  (put 'project '(complex) project)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define i (make-scheme-number 2))
(define r (make-real 2.0))
(define ra (make-rational 1 2))
(define c (make-complex-from-real-imag 1 3))