#lang sicp

; Creates pair (x, y).
;
; Parameters:
; x - first element of pair
; y - second element of pair
;
; Returns:
; pair (x, y)
(define (cons x y)
  (lambda (m) (m x y)))

; Gets first element of pair.
;
; Parameters:
; z - pair
;
; Returns:
; first element of pair
(define (car z)
  (z (lambda (p q) p)))

; (car (cons x y))
; -> (car (lambda (m) (m x y)))
; -> ((lambda (m) (m x y)) (lambda (p q) p))
; -> ((lambda (p q) p) x y)
; -> x

; Gets second element of pair.
;
; Parameters:
; z - pair
;
; Returns:
; second element of pair
(define (cdr z)
  (z (lambda (p q) q)))