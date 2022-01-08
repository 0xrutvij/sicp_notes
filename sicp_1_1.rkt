#lang sicp

;; 1.1

;; The distinction between normal and applicative order evaluation
;; A few functions for the demo

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; Normal-Order evaluation
;; Substitute until only primitive operators remain
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
;; Once primitive operators' stage reached, reduce
(+ (* 6 6) (* 10 10))
(+ 36 100)

;; Applicative-Order evaluation
;; evaluate all parameters (reduction) before substitution - recursively till atom is reached
(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))
(+ (* 6 6) (* 10 10))
(+ 36 100)

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Ex. 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x) (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.0000000001) )

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 36)