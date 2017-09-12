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

; Reverses a list.
;
; Arguments:
; l - reversed list
;
; Returns:
; reversed list
(define (reverse l)
  (let ((head (car l))
        (tail (cdr l)))
    (if (null? tail)
        (cons head tail)
        (push-back (reverse tail) head))))

; Recursively reverses a list and elements of a list that are lists.
;
; Examples:
;    (deep-reverse nil) == nil
;    (deep-reverse 1) == 1
;    (deep-reverse (list 1 2 3)) == (list 3 2 1)
;    (deep-reverse (list (list 1 2 3))) == (list (list 3 2 1))
;    (deep-reverse (list (list (list 1 2 3)))) == (list (list (list 3 2 1)))
;
; Parameters:
; l - list to reverse
;
; Returns:
; reversed list
(define (deep-reverse l)
  (cond ((not (pair? l)) l)
        ((null? l) l)
        (else
         (let ((head (car l))
               (tail (cdr l)))
           (push-back (deep-reverse tail) (deep-reverse head))))))

(equal? (deep-reverse nil) nil)
(equal? (deep-reverse (list 1)) (list 1))
(equal? (deep-reverse (list 1 2 3)) (list 3 2 1))
(equal? (deep-reverse (list (list 1 2 3))) (list (list 3 2 1)))
(equal? (deep-reverse (list (list (list 1 2 3)))) (list (list (list 3 2 1))))

