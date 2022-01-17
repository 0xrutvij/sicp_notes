#lang sicp
;; 2.3 Symbolic Data


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;; 2.3.1 - Quotation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; examples of memq

#|
(memq 'apple '(pear banana prune))

(memq 'apple '(x(apple sauce) y apple pear))
|#

;; Ex 2.53

;(list 'a 'b 'c)
;'(a b c)
;
;
;(list (list 'george))
;'((george))
;
;(cdr '((x1 x2) (y1 y2)))
;'((y1 y2))
;
;(cadr '((x1 x2) (y1 y2)))
;'(y1 y2)
;
;(pair? (car '(a short list)))
;#f
;
;(memq 'red '((red shoes) (blue socks)))
;#f
;
;(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)


;; Ex 2.54

(define (equal.? l1 l2)
  (define (iter l1 l2)
    (let ((nl1 (null? l1))
          (nl2 (null? l2)))
      (cond
        ((and nl1 nl2) #t)
        ((or nl1 nl2) #f)
        ((eq? (car l1) (car l2)) (iter (cdr l1) (cdr l2)))
        (else #f))))
  (iter l1 l2))



;; Ex 2.53

;(car ''abracadabra)

;(car '(quote abracadabra))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;; 2.3.2 - Symbolic Differentiation  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|

Rules:

- Only addition and multiplication with two arguments

- dc/dx = 0 for c a constant or some variable not equal to x

- dx/dx = 1

- d(u + v)/dx = du/dx + dv/dx

- d(uv)/dx = u*(dv/dx) + v*(du/dx)

- d(u**n)/dx = n*(u**(n-1))*(du/dx)

|#

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (op-expr? op-type expr) (and (pair? expr) (eq? (car expr) op-type)))

(define (first-operand expr) (cadr expr))

(define (second-operand expr) (caddr expr))

(define (sum? x) (op-expr? '+ x))

(define (addend s) (first-operand s))

(define (augend s) (second-operand s))

(define (product? x) (op-expr? '* x))

(define (multiplier s) (first-operand s))

(define (multiplicand s) (second-operand s))


(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)


;; Ex 2.56

;; Refer to line 88 for the rule addition

