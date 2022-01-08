#lang sicp
;; 1.2 - The "test patterns of photography analogy" - oversimplifed prototypical patterns

(define (square x) (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; 1.2.1 - Linear recursion and iteration ;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tail-Recursion: the fact that the recursive callee is not assigned a stack
;; frame and utilizes the stack of the same function.

;; Factorial - w/o taking advantage of tail call optimization

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; a chain of deferred operations build up
; (factorial 4)
; (* 4 (factorial 3))
; (* 4 (* 3 (factorial 2)))
; (* 4 (* 3 (* 2 (factorial 1))))
; (* 4 (* 3 (* 2 1)))
; (* 4 (* 3 2))
; (* 4 6)
; 24
;; (a) a recursive process - a chain of deferred ops
;; (b) a linear recursive process - grows linearly with size of input



;; Iterative, tail-call optimized factorial

(define (factorial1 n)
(define (iter product counter)
(if (> counter n) product
        (iter (* counter product)
              (+ counter 1))))
(iter 1 1))

; (factorial_tco 4)
; (fact-iter 1 1)
; (fact-iter 1 2)
; (fact-iter 2 3)
; (fact-iter 6 4)
; (fact-iter 24 5)
; 24

;; (a) an iterative process - state summarized by a fixed number of state variables
;;                          - no growth or shrinkage, thus can use the stack of callee
;;
;; (b) a linear iterative process - since the number of steps grow linearly w/ no. of inputs

;; special iteration contstructs are useful only as syntactic sugar, since tail-recursion will
;; execute an iterative process, even one described by a recursive procedure in constant space.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        ('t (A (- x 1) (A x (- y 1))))))

;; (A 1 10)
;; (A 2 4)
;; (A 3 3)

(define (f n) (A 0 n)) ; (* 2 n)
(define (g n) (A 1 n)) ; (expt 2 n)
(define (h n) (A 2 n)) ; (expt 2 (expt 2 (expt 2 ... n-1 times ;; 2^(2^(2^2

(define (iter-call func n ls)
  (cond ((= n -1) ls)
        (else (iter-call func (- n 1) (cons (func n) ls)))))

;; (iter-call f 5 '()) 

;; (iter-call g 5 '())

;; (iter-call h 4 '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 1.2.2 - Tree Recursion ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define phi (/ (+ 1 (sqrt 5)) 2))

;; (= (+ phi 1) (* phi phi))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (count-change amount) (cc amount 5))

;; (count-change 10)


;; fib direct formula approximation
(define (fib n) (round (/ (expt phi n) (sqrt 5))))


;; pascal number - binomial coeffients
(define (pascal n) (cond ((= n 0) '(1))
                         ('t (pasc-iter n '(1 1)))))

(define (pasc-iter n ls)
  (cond ((= n 1) ls)
        ('t (pasc-iter (- n 1) (cons 1 (append (psum ls '()) '(1)))))))

(define (psum ls ps)
    (cond ((>= (length ls) 2) (psum (cdr ls) (cons (+ (car ls) (car (cdr ls))) ps)))
          ('t ps)))

;; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.0001))
      angle
      (p (sine (/ angle 3.0)))))

;(define pi 3.14159)

;; 4 r^2 - area of square bounding a circle of radius r
;; pi r^2 - area of the aforementioned circle
;; num points in circle / num points in square = pi / 4
;; pi = 4 * (npc / nps)

;; (0, 0) --> (1000, 1000)
;; if (x-500)^2 + (y-500)^2 <= (500)^2

(define (pt-in-circle? x y xo yo r) (<= (+ (square (- x xo))
                                           (square (- y yo)))
                                        (square r)))

(pt-in-circle? 500 500 500 500 500)

(define (pt-in-circle5000c5000r? x y) (pt-in-circle? x y 5000 5000 5000))

(define (iter-inner f n m ls)
  (cond ((= m -1) ls)
        ('t (iter-inner f n (- m 1) (+ ls (if (f n m)
                                              1.0
                                              0.0))))))

(define (iter-outer f n m ls)
  (cond ((= n -1) ls)
        ('t (iter-outer f (- n 1) m (+ ls (iter-inner f n m 0))))))

; (define npc (iter-outer pt-in-circle5000c5000r? 10000 10000 0))
; (define nps (* 10000 10000))
; (define pi (* 4.0 (/ npc nps)))
; pi
; (sine (/ pi 2))
; (sine (/ pi 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 1.2.5 - Greatest Common Divisor ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GCD 206, 40 = GCD 40, 6 - GCD a, b = b a/b

(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 1.2.6 - Primality Testing ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ('t (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (iter-call2 func n ls)
  (cond ((= n 9900) ls)
        (else (iter-call2 func (- n 1) (if (func n) (cons n ls) ls)))))

;(iter-call2 prime? 10000 '())

;; Fermat's Little Theorem - Given n is prime and a is any positive integer less than n
;; then a^n % n == a

