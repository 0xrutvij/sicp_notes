#lang sicp
;; 2.1

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define (squared-difference c1 c2)
  (square (- c1 c2)))

(define (log-base base num)
  (/ (log num) (log base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;; 2.1.1 - Arithmetic for Rational Numbers  ;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) d)))
    (cons (/ n g) (abs (/ d g)))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat
   (+  (* (numer x) (denom y))
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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;; 2.1.2 - Abstraction Barriers  ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ex 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (mid-point seg)
  (let ((sp (car seg))
        (ep (cdr seg)))
    (make-point
     (average (x-point sp) (x-point ep))
     (average (y-point sp) (y-point ep)))))

(define (length seg)
  (let ((sp (start-segment seg))
        (ep (end-segment seg)))
    (sqrt (+
           (squared-difference (x-point sp) (x-point ep))
           (squared-difference (y-point sp) (y-point ep))))))
    

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Ex 2.3 Rectangles in a plane

(define (rectangle left bottom)
  (cons left bottom))

(define (rect-base rect)
  (car rect))

(define (rect-height rect)
  (cdr rect))

(define (rectangle-4 ld lu ru rd)
  (cons (make-segment ld rd)
        (make-segment ld lu)))

(define (area-rect rect)
  (let ((seg1 (rect-base rect))
        (seg2 (rect-height rect)))
  (* (length seg1) (length seg2))))

(define (perimeter-rect rect)
  (let ((seg1 (rect-base rect))
        (seg2 (rect-height rect)))
  (+ (* 2 (length seg1)) (* 2 (length seg2)))))

;; Procedures as objects - message passing. (aka procedural representations of data)

#|

(define (cons. x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car. z) (z 0))
(define (cdr. z) (z 1))

|#


;; Ex 2.4 - Alternative procedural representation of pairs

(define (cons. x y)
  (lambda (m) (m x y)))

(define (car. z)
  (z (lambda (p q) p)))

(define (cdr. z)
  (z (lambda (p q) q)))

;; Ex 2.5

(define (cons-pos a b) (* (expt 2 a) (expt 3 b)))

(define (car-pos p)
  (define (iter val)
    (if (= 0 (remainder val 3))
        (iter (/ val 3))
        val))
  (log-base 2 (iter p)))

(define (cdr-pos p)
  (define (iter val)
    (if (= 0 (remainder val 2))
        (iter (/ val 2))
        val))
  (log-base 3 (iter p)))


;; Ex 2.6 - Church Numerals


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;; 2.1.4 - Interval Arithmetic  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Later
