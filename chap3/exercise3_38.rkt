#lang sicp

; (define (balance 100))

; A Piotr (set! balance (+ balance 100))
; B Pawel (set! balance (- balance 20))
; C Maria (set! balance (- balance (/ balance 2)))

; a)

; A->B->C = 90
; A->C->B = 80
; B->A->C = 90
; B->C->A = 140
; C->A->B = 130
; C->B->A = 130

; b)
; ...
