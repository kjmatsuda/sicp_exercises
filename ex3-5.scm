;; (define (make-point x y)
;;   (cons x y))

;; (define (make-rect lower-point upper-point)
;;   (cons lower-point upper-point))

;; (define (lower-point rect)
;;   (car rect))

;; (define (upper-point rect)
;;   (cdr rect))

;; (define (x-value point)
;;   (car point))

;; (define (y-value point)
;;   (cdr point))

;; (define (rect-area rect)
;;   (* (- (x-value (upper-point rect))
;; 	(x-value (lower-point rect)))
;;      (- (y-value (upper-point rect))
;; 	(y-value (lower-point rect)))))

;; (define (estimate-integral predicate rect trials)
;;   (* (rect-area rect) (monte-carlo trials predicate))

;; ;; なんでこんなややこしい定義になってしまったのか？
;; (define (circle-test center radius)
;;   (>= (square radius) 
;;       (+ (square (- (random-in-range (x-value (lower-point rect))
;; 				     (x-value (upper-point rect)))
;; 		    (x-value center)))
;; 	 (square (- (random-in-range (y-value (lower-point rect))
;; 				     (y-value (upper-point rect)))
;; 		    (y-value center))))))

(load "./arithmetic-util.scm")
;; やっぱり円を中心に考えていこう
(define (make-circle center radius)
  (cons center radius))

(define (center circle)
  (car circle))

(define (radius circle)
  (cdr circle))

(define (upper-x-coordinate circle)
  (+ (x-coordinate (center circle))
     (radius circle)))

(define (upper-y-coordinate circle)
  (+ (y-coordinate (center circle))
     (radius circle)))

(define (lower-x-coordinate circle)
  (- (x-coordinate (center circle))
     (radius circle)))

(define (lower-y-coordinate circle)
  (- (y-coordinate (center circle))
     (radius circle)))

(define (x-coordinate point)
  (car point))

(define (y-coordinate point)
  (cdr point))

(define (rect-area circle)
  (* (- (upper-x-coordinate circle)
	(lower-x-coordinate circle))
     (- (upper-y-coordinate circle)
	(lower-y-coordinate circle))))

(define (estimate-integral circle trials)
  (* (rect-area circle) (monte-carlo trials (lambda ()
					      (circle-test circle)))))

(define (circle-test circle)
  (>= (square (radius circle)) 
      (+ (square (- (random-in-range (lower-x-coordinate circle)
				     (upper-x-coordinate circle))
		    (x-coordinate (center circle))))
	 (square (- (random-in-range (lower-y-coordinate circle)
				     (upper-y-coordinate circle))
		    (y-coordinate (center circle)))))))
      
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))