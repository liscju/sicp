#lang sicp

; Creates new list by applying function to each item on
; the specified list.
;
; The following equation holds:
; map f nil = nil
; map f (list x1 x2 x3 ...) = (list (f x1) (f x2) (f x3) ..)
;
; Arguments:
; proc - function to apply to each item on the list
; items - list of items
;
; Returns:
; new list produced by applying procedure to each element
; of items
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Creates list being copy of the specified list with each
; item squared (recursive version).
;
; Arguments:
; items - list of items
;
; Returns:
; list of squared items
(define (square-list-basic items)
  (if (null? items)
      nil
      (cons (let ((x (car items))) (* x x)) (square-list-basic (cdr items)))))

; Creates list being copy of the specified list with each
; item squared (map version).
;
; Arguments:
; items - list of items
;
; Returns:
; list of squared items
(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

