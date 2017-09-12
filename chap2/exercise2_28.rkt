#lang sicp

; Adds element to the end of the list.
;
; Arguments:
; l - list
; x - element to add
;
; Returns:
; list l with pushed element x on the end
(define (push-back l x)
  (if (null? l)
      (cons x nil)
      (let ((head (car l))
            (tail (cdr l)))
        (if (null? tail)
            (cons head (cons x nil))
            (cons head (push-back tail x))))))

; Extends list with elements from other list.
;
; Examples:
;    (extend-list nil nil) == nil
;    (extend-list (list 1) nil) == (list 1)
;    (extend-list nil (list 1)) == (list 1)
;    (extend-list (list 1) (list 2)) == (list 1 2)
;    (extend-list (list 1 2) (list 3 4)) == (list 1 2 3 4)
;
; Parameters:
; l - list
; o - other list
;
; Returns;
; list extends with element from other list
(define (extend-list l o)
  (cond ((null? l) o)
        ((null? o) l)
        (else
         (let ((ohead (car o))
               (otail (cdr o)))
           (extend-list (push-back l ohead) otail)))))

(equal? (extend-list nil nil) nil)
(equal? (extend-list (list 1) nil) (list 1))
(equal? (extend-list nil (list 1)) (list 1))
(equal? (extend-list (list 1) (list 2)) (list 1 2))
(equal? (extend-list (list 1 2) (list 3 4)) (list 1 2 3 4))

; Collects leafs of the tree into list.
;
; Examples:
;    (fringe nil) == nil
;    (fringe (list 1)) == (list 1)
;    (fringe (list 1 2)) == (list 1 2)
;    (fringe (list (list 1 2) (list 3 4))) == (list 1 2 3 4)
;
; Parameters:
; t - tree
;
; Returns:
; leafs of the tree as a list
(define (fringe t)
  (cond ((null? t) nil)
        ((not (pair? t)) (cons t nil))
        (else
         (let ((head (car t))
               (tail (cdr t)))
           (extend-list (fringe head) (fringe tail))))))

(equal? (fringe nil) nil)
(equal? (fringe (list 1)) (list 1))
(equal? (fringe (list 1 2)) (list 1 2))
(equal? (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4))
