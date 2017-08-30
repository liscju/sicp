#lang sicp

; Church numeral 0.
(define zero (lambda (f) (lambda (x) x)))

; Add one to church numeral.
;
; Parameters:
; n - church numeral
;
; Returns:
; 
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Church numeral 1.
;---------------------------------------------
;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f x)))
;---------------------------------------------
(define one (lambda (f) (lambda (x) (f x))))

; Church numeral 2.
;---------------------------------------------
;(add-1 one)
;(add-1 (lambda (f) (lambda (x) (f x))))
;((lambda (f) (lambda (x) (f (f x)))))
;---------------------------------------------
(define two (lambda (f) (lambda (x) (f (f x)))))

; Adds two church numerals.
;
; Parameters:
; n1,n2 - church numerals
;
; Return:
; sum of n1 and n2
(define (add n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))