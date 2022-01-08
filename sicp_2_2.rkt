#lang sicp
;; 2.2 - Hierarchical Data & the Closure Property of Cons

#|
Elements are closed under the cons operator

Pairing pairs yields a pair again.

Just like rational numbers are closed under addition, where
addition of rational numbers gives another rational number.

Such closure under cons allows for the creation of hierarchical structs
where structures are recursively made of other structures as their parts
|#

(define atom? (lambda (x)
                (and (not (null? x)) (not (pair? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;; 2.2.1 - Representing Sequences  ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   List is cons of pairs, where first part of each pair is the element
   and the second part is a pointer to the next pair. For the terminating
   pair, the second part is a pointer to nil or the empty list '()
|#

; (cons 1 (cons 2 (cons 3 (cons 4 '()))))

; is the same as

; (list 1 2 3 4)

(define (list-ref. items n)
  (if (= n 0)
      (car items)
      (list-ref. (cdr items) (- n 1))))

(define (length. items)
  (define (iter len ls)
    (if (null? ls)
        len
        (iter (+ len 1) (cdr ls))))
  (iter 0 items))

(define (append. list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Ex 2.17

(define (last-pair ls)
  (let ((ls-len (length. ls)))
    (cond
      ((= ls-len 0) '())
      ((= ls-len 1) (car ls))
      ('t (last-pair (cdr ls))))))

;; Ex 2.18

(define (reverse ls)
  (define (iter curr rev)
    (if (null? curr)
        rev
        (iter (cdr curr) (append (cons (car curr) '()) rev))))
  (iter ls '()))

;; Ex 2.20

(define (same-parity . nums)
  (let* ((first (car nums))
        (rest (cdr nums))
        (parity (remainder first 2)))
    (define (iter ret res)
      (if (null? res)
          ret
          (iter
           (append
            ret
            (if (= parity (remainder (car res) 2))
                (list (car res))
                '())
            )
           (cdr res)
           )))
    (iter (list first) rest)))


;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor) (scale-list (cdr items) factor))))

(define (map. proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map. proc (cdr items)))))

;; Ex 2.21

(define (square-list items)
  (map. (lambda (x) (* x x)) items))


;; Ex 2.23

(define (for-each. proc items)
  (if (not (null? items))
      (and (proc (car items)) (for-each. proc (cdr items)))))


;; Hierarchical Structures

(define (count-leaves tree)
  (cond
    ((null? tree) 0)
    ((list? (car tree)) (+ (count-leaves (cdr tree)) (count-leaves (car tree))))
    (else (+ 1 (count-leaves (cdr tree))))))


;; Ex 2.25

; (cadr (cadr (cdr '(1 3 (5 7) 9))))

; (caar '((7)))

; (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))



;; Ex 2.27

(define (deep-reverse ls)
  (cond
    ((null? ls) ls)
    ((list? (car ls)) (append (deep-reverse (cdr ls)) (list (deep-reverse (car ls)))))
    ('t (append (deep-reverse (cdr ls)) (list (car ls))))))


;; Ex 2.28

(define (fringe tree)
  (cond
    ((null? tree) tree)
    ((list? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
    ('t (cons (car tree) (fringe (cdr tree))))))


;; Ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (atom? mobile)
      mobile
      (let*
          ((lb (left-branch mobile))
           (rb (right-branch mobile))
           (lb-str (branch-structure lb))
           (rb-str (branch-structure rb)))
        (+ (total-weight lb-str) (total-weight rb-str)))))

(define (balanced? mobile)
  (if (atom? mobile)
      't
      (let*
          ((lb (left-branch mobile))
           (rb (right-branch mobile))
           (lb-len (branch-length lb))
           (rb-len (branch-length rb))
           (lb-str (branch-structure lb))
           (rb-str (branch-structure rb)))
        (and
         (balanced? lb-str)
         (balanced? rb-str)
         (= (* lb-len (total-weight lb-str))
            (* rb-len (total-weight rb-str)))))))


(define m1
  (make-mobile
   (make-branch
    10
    (make-mobile
     (make-branch 10 10) (make-branch 10 20)))
   (make-branch
    30
    (make-mobile
     (make-branch 10 10) (make-branch 1 40)))))

(define bm
  (make-mobile (make-branch 10 10) (make-branch 10 10)))

(define bm1
  (make-mobile (make-branch 100 bm) (make-branch 100 bm)))

(define bm2
  (make-mobile (make-branch 1000 bm1) (make-branch 1000 bm1)))


;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; Ex 2.30

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; Ex 2.31

(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (sq-tree tree) (tree-map (lambda (x) (* x x)) tree))


;; Ex 2.32 - Powerset

(define (powerset s)
  (if (null? s)
      (list '())
      (let ((rest (powerset (cdr s))))
        (append rest (map (lambda (z) (cons (car s) z)) rest)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;; 2.2.3 - Sequences As Conventional Interfaces ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


;; Sum the square of odd valued leaves of a tree

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map (lambda (x) (* x x)) (filter odd? (enumerate-tree tree)))))


;; Get the even fibonnaci numbers from first n fib numbers

(define (fib n)
  (define (fib-iter i v v-1)
    (if (= i n)
        v-1
        (fib-iter (+ i 1) (+ v v-1) v)))
  (fib-iter 0 1 0))

(define (even-fibs n)
  (accumulate
   cons
   '()
   (filter even? (map fib (enumerate-interval 0 n)))))

;; Ex 2.33

(define (map-233 func sequence)
  (accumulate (lambda (x y) (cons (func x) y)) nil sequence))

(define (append-233 sq1 sq2)
  (accumulate cons sq2 sq1))

(define (len-233 seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;; Ex 2.34 ~ Polynomial Evaluation

(define (horner-eval x coeff-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coeff-sequence))

;; Ex 2.35 ~ Count-Leaves

(define (count-leaves. tree)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree))))


;; Ex 2.36 ~ Accumulate-N

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; Ex 2.37 ~ Vector & Matrix Math

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


;; Ex 2.38 ~ Fold-Right (accumulate) & Fold-Left

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (foldr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (foldr op initial (cdr sequence)))))

;; Ex 2.39 ~ Foldl & Foldr for reverse

(define (reverse-r sequence)
  (foldr (lambda (x y) (cons x y)) nil sequence))

(define (reverse-l sequence)
  (foldl (lambda (x y) (cons y x)) nil sequence))



;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define pair-10 (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 10)))


;; Permutations

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))


;; Ex 2.40

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Ex 2.41

(define (unique-triplets n)
  (flatmap (lambda (x) (map (lambda (pr) (cons n pr)) (unique-pairs (- x 1))))
           (enumerate-interval 1 n)))


;; Ex 2.42 ~ N-Queens Puzzle

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))


(define (not-same-col ax bx)
  (not (= ax bx)))

(define (not-same-row ay by)
  (not (= ay by)))

(define (not-on-diag ax ay bx by)
  (not (= (abs (- ax bx)) (abs (- ay by)))))
  


(define (check a b)
  (let ((ax (car a))
        (ay (cadr a))
        (bx (car b))
        (by (cadr b)))
    (and (not-same-col ax bx)
         (not-same-row ay by)
         (not-on-diag ax ay bx by))))

(define (safe? y)
  (= 0 (accumulate + 0
                 (map (lambda (x)
                        (if (check (car y) x) 0 1))
                      (cdr y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;; 2.2.4 - Example: A Picture Language ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

