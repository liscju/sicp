#lang sicp

; Adds element to the end of the list.
;
; Arguments:
; l - non empty list
; x - element to add
;
; Returns:
; list l with pushed element x on the end
(define (push-back l x)
  (let ((head (car l))
        (tail (cdr l)))
    (if (null? tail)
        (cons head (cons x nil))
        (cons head (push-back tail x)))))

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
