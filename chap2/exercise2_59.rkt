#lang sicp

; Checks whether x is an element of set.
;
; Parameters:
; x - element whose presence in the set it to be tested
; set - set to check if x belongs to it
;
; Returns:
; true when x is an element of set, false otherwise
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(not (element-of-set? 5 (list)))
(not (element-of-set? 5 (list 6)))
(element-of-set? 5 (list 5))
(element-of-set? 5 (list 2 5 4))
(element-of-set? 5 (list 1 2 5))

; Add element to set.
;
;
; Parameters:
; x - element to add
; set - set to add element
;
; Returns:
; set with added element
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(equal? (adjoin-set 5 (list)) (list 5))
(equal? (adjoin-set 5 (list 1)) (list 5 1))
(equal? (adjoin-set 5 (list 5)) (list 5))
(equal? (adjoin-set 5 (list 5 3 2)) (list 5 3 2))
(equal? (adjoin-set 5 (list 2 5 3)) (list 2 5 3))
(equal? (adjoin-set 5 (list 2 3 5)) (list 2 3 5))
(equal? (adjoin-set 5 (list 1 2 3)) (list 5 1 2 3))

; Compute intersection of sets.
;
; Parameters:
; set1 - first set
; set2 - second set
;
; Returns:
; intersection of sets
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(equal? (intersection-set (list) (list)) (list))
(equal? (intersection-set (list) (list 1)) (list))
(equal? (intersection-set (list 1) (list)) (list))
(equal? (intersection-set (list 1) (list 2)) (list))
(equal? (intersection-set (list 1 2) (list 2 1)) (list 1 2))
(equal? (intersection-set (list 1 2 3) (list 5 4 1)) (list 1))
(equal? (intersection-set (list 1 2 3) (list 4 2 5)) (list 2))
(equal? (intersection-set (list 1 2 3) (list 5 4 3)) (list 3))
(equal? (intersection-set (list 1 2 3 4 5) (list 5 6 3 8 1)) (list 1 3 5))

; Creates union of the two sets.
;
; Union of two sets is a set that contains all element from both sets.
;
; Parameters:
; set1 - first set
; set2 - second set
;
; Returns:
; union of both sets
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(equal? (union-set (list) (list)) (list))
(equal? (union-set (list 1 2) (list)) (list 1 2))
(equal? (union-set (list) (list 1 2)) (list 1 2))
(equal? (union-set (list 1 2) (list 3 4)) (list 1 2 3 4))
(equal? (union-set (list 1 2) (list 1 2)) (list 1 2))
(equal? (union-set (list 1 2 3) (list 4 1 5)) (list 2 3 4 1 5))
(equal? (union-set (list 1 2 3) (list 4 2 5)) (list 1 3 4 2 5))
(equal? (union-set (list 1 2 3) (list 3 4 5)) (list 1 2 3 4 5))
(equal? (union-set (list 1 2 3 4 5) (list 1 6 3 7 5)) (list 2 4 1 6 3 7 5))