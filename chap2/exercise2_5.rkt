#lang sicp

; Calculates base to the power of specified exponent.
;
; Parameters:
; x - base
; n - exponent
;
; Returns:
; base to the power of exponent
(define (pow x n)
  (cond ((= n 0) 1)
        (else (* x (pow x (dec n))))))

; Creates pair (x, y).
;
; Parameters:
; x - first element of pair
; y - second element of pair
;
; Returns:
; pair (x, y)
(define (cons x y)
  (* (pow 2 x) (pow 3 y)))

; Gets first element of pair.
;
; Parameters:
; z - pair
;
; Returns:
; first element of pair
(define (car z)
  (if (not (= (remainder z 2) 0))
      0
      (inc (car (/ z 2)))))

; Gets second element of pair.
;
; Parameters:
; z - pair
;
; Returns:
; second element of pair
(define (cdr z)
  (if (not (= (remainder z 3) 0))
      0
      (inc (cdr (/ z 3)))))
