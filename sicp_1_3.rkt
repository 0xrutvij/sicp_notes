#lang sicp
;; 1.3 Formulating Abstractions with Higher-Order Procedures

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y) (/ (+ x y) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; 1.3.1 - Procedures as Arguments ;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A generalized summation

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b) (sum (lambda (x) (* x x x)) a (lambda (x) (+ x 1)) b))

;(sum-cubes 1 2)

(define (pi-sum a b)
  (sum
   (lambda (x) (/ 1.0 (* x (+ x 2))))
   a
   (lambda (x) (+ x 4))
   b))

;(* 8 (pi-sum 1 100000))

;; Integrals

; integral from a to b of f = [ f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2) + ...] dx

(define (integral f a b dx)
  (* dx
     (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)))

; (integral cube 0 1 0.01)

; (integral cube 0 1 0.001)

;; Ex 1.29
; integral from a to b of f = h/3 * [y0 + 4y1 + 2y2 + 4y3 ... + 4yn-1 + yn]
; Simpson's rule. Where h = (b-a)/n & y_k = f(a+kh) where f is the original integrand.

(define (h a b n) (/ (- b a) n))

(define (y-k f a b k n) (f (+ a (* k (h a b n)))))

(define (yp-k f a b k n)
  (define yi (y-k f a b k n))
  (cond
    ((or (= 0 k) (= n k)) yi)
    ((= 0 (remainder k 2)) (* 2 yi))
    ('t (* 4 yi))))

(define (simpsons-integral f a b even-int)
  (*
   (sum
   (lambda (x) (yp-k f a b x even-int))
   0
   (lambda (x) (+ x 1))
   even-int)
   (/ (h a b even-int) 3)))

; (simpsons-integral cube 0 1 100)
; (simpsons-integral cube 0 1 1000)

;; Ex 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (I x) x)
(define (sum-integers a b)
  (sum-iter I a (lambda (x) (+ x 1)) b))

; (sum-integers 1 10)

;; Ex 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product
   I
   1
   (lambda (x) (+ x 1))
   n))

; (factorial 6)

; (a) approximation of pi using Wallis' formula

(define (incr ls)
  (define n (car ls))
  (define d (cadr ls))
  (if (> n d)
      (list n (+ d 2))
      (list (+ n 2) d)))

(define (nt i ls n)
    (if (= i n) ls (nt (+ i 1) (incr ls) n)))

(define (get-nth k)
  (nt 1 (list 2 3) k))

(define (f-nth k)
  (let* ((ls (get-nth k))
  (n (car ls))
  (d (cadr ls)))
  (/ n d)))

;(define pi-by-4 (product f-nth 1 (lambda (x) (+ x 1)) 100))

;(* pi-by-4 4.0)

;; Ex 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter curr acca)
    (if (> curr b)
        acca
        (iter (next curr) (combiner acca (term curr)))))
  (iter a null-value))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;; 1.3.3 - Procedures as General Methods ;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search f neg-point pos-point)
  (let ((midpoint (/ (+ pos-point neg-point) 2)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          ('t (error "Values are not of opposite sign" a b)))))


;; Fixed points of functions --> fixed point combinators --> Y/Z Combinator
;; A number x is called a fixed point of a function if ||>> f(x) = x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)

;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;; Uses average damping. Instead of find fix of  y |-> x/y
;; we find the fix of y |-> 1/2 (y + x/y)
(define (sqrt x) (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Ex 1.35

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


;; Ex 1.36

(define (vfixed-point func first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (func guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; fix of x^x 1000 i.e
;; x |-> log(1000)/log(x) without damping
;(vfixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

;; with damping
;(newline)
;(display "fixed-point search for x |-> log(1000)/log(x) with damping")
;(vfixed-point (lambda (y) (average y ((lambda (x) (/ (log 1000) (log x))) y))) 1.1)

;; Ex 1.37 (Continued Fraction)

(define (cont-frac n d k)
  (define (rec-h i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec-h (+ i 1))))))
  (rec-h 1))

; Golden ratio as a continued-fraction
; (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))

; Golden ratio (1 + √5) / 2
; (/ (+ 1 (sqrt 5)) 2)


;; Ex 1.38 (Euler's continued fraction for e - 2)

(define (ne-i i) 1)
(define (de-i i)
  (let ((mod3 (remainder i 3))
         (div3 (ceiling (/ i 3))))
    (if (= mod3 2)
        (* 2.0 div3)
        1.0)))


(define (cont-frac-iter n d k)
  (define (iter k result)
    (cond ((zero? k) result)
          (else (iter (- k 1) (/ (n k) (+ (d k) result))))))
  (iter k 0))

#|

(define (e-euler) (+ 2 (cont-frac ne-i de-i 100)))
(newline)
(display "e with Euler's continued fraction approximation, using recursive process: ")
(e-euler)

(newline)
(display "e with Euler's continued fraction approximation, using iterative process: ")
(define (euler-e) (+ 2 (cont-frac-iter ne-i de-i 100)))
(euler-e)

(newline)
(display "Original e: ")
(exp 1)

(newline)
(display "Golden ratio using continue fraction, recursive: ")
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))
(newline)
(display "Golden ratio using continue fraction, iterative: ")
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100))

(newline)
(display "Original golden ratio: ")
(/ (+ 1 (sqrt 5)) 2)

|#




;; Ex 1.39 tan(x) approximation using a continued fraction.

(define (nlt-i-x i x)
  (cond ((= i 1) (- x))
        ('t (- (expt x 2)))))

(define (dlt-i i)
  (- (* 2 i) 1))

(define (tan-cf x k)
  (- (cont-frac
   (lambda (i) (nlt-i-x i x))
   dlt-i
   k)))

;(tan-cf (/ 3.1415 4) 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; 1.3.4 - Procedures as Return Values ;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Damping averages in general

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Using fixed-points and damping to find roots

; (1) square root
; y^2 = x
; Fixed point of y ↦ x/y

(define (square-root x) (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))


; (2) cube root
; y^3 = x
; Fixed point of y ↦ x/(y^2)

(define (cube-root x) (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))



;(square-root (cube-root (square-root 64)))



#| ######################### Newton's method ######################### |#

#|

If x ↦ g(x)

is a differentiable function, then a solution of the equation g(x) = 0
is a fixed point of the function x ↦ f(x), where 

f(x) = x - g(x) / Dg(x) and Dg(x) is the derivative of g evaluated at x

Dg(x) = [g(x + dx) - g(x)] / dx

|#

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (square-root2 x) (newtons-method (lambda (y) (- (square y) x)) 1.0))

;(square-root2 64)

;; Fixed-Point Transforms

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (square-root3 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (square-root4 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

#|

First Class Elements of a Language
- can be named by variables
- can be passed as arguments to procedures
- can be returned as results of procedures
- can be included in data structures

|#


;; Ex 1.40

(define (cubic a b c)
  (lambda (x)
    (+
     (cube x)
     (* a (square x))
     (* b x)
     c)))

;(newtons-method (cubic 4 1 1) 1)

;; Ex 1.41

(define (double func)
  (lambda (x) (func (func x))))

; (((double (double double)) inc) 5)


;; Ex 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; ((compose square inc) 6)


;; Ex 1.43

(define (repeated func n)
  (cond ((= n 1) (lambda (x) (func x)))
        ('t (compose func (repeated func (- n 1))))))

;((repeated square 2) 5)


;; Ex 1.44

; Using the dx defined at line 351

(define (smooth f) (lambda (x) (/ (+
                                   (f x)
                                   (f (+ x dx))
                                   (f (- x dx))) 3)))

(define (smooth-n n) (repeated smooth n))

(define smoothed-sin ((smooth-n 13) sin))

;(smoothed-sin (/ 3.14 2))
;(sin (/ 3.14 2))


;; Ex 1.45

(define (damp-fourth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y))) average-damp 1.0))

; (damp-fourth-root 16) ;; Doesn't converge

(define (dd4r x)
  (fixed-point-of-transform
   (lambda (y) (/ x (cube y)))
   (compose average-damp average-damp)
   1.0))

; (dd4r 16) ;; Converges, 4th root needs double damping

(define (n-1-pow x n)
  (define (iter s res)
    (cond ((= s 1) res)
          ('t (iter (- s 1) (* res x)))))
  (iter n 1))

(define logB
  (lambda (x B)
    (/ (log x) (log B))))

(define (nth-root n)
  (lambda (x)
    (fixed-point-of-transform
     (lambda (y) (/ x (n-1-pow y n)))
     (repeated average-damp (floor (logB n 2)))
     1.0)))

; ((nth-root 20) 1048576)


;; Ex 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter curr-guess)
      (if (good-enough? curr-guess)
          curr-guess
          (iter (improve curr-guess))))
    (iter guess)))

(define (sqroot x) ((iterative-improve
                    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                    (lambda (guess) (average guess (/ x guess))))
                    1.0))
; (sqroot 841)

(define (fixed-pt f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- (f guess) (f (f guess)))) 0.00001))
    (lambda (guess) (f guess)))
    first-guess))


(define (sqt x) (fixed-pt (lambda (y) (average y (/ x y))) 1.0))

(sqt 998001)