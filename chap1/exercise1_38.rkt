#lang sicp

; Calculates finite continued fraction with specified coefficients and depth.
;
; Finite continued fraction has the following form:
;
;        N_{1}
; -------------------------------------
;  D_{1} +    N_{2}
;           ---------------------------
;             D_{2} + ....
;
;                                 N_{k}
;              ...........  +    -------
;                                 D_{k}
;
; Parameters:
; n - coefficient function of index argument
; d - coefficient function of index argument
; k - depth of fraction
;
; Returns:
; Value of fraction
;
(define (count-frac n d k)
  (define (count-frac-iter i)
    (cond ((> i k) 0)
           ((= i k) (/ (n i)
                       (d i)))
           ((< i k) (/ (n i)
                       (+ (d i) (count-frac-iter (inc i)))))))
  (count-frac-iter 1))

(define (n i)
  1)

; d(i) = {1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...}
(define (d i)
  (cond ((= i 1) 1)
        ((= i 2) 2)
        ((= (remainder (- i 2) 3) 0) (+ 2(* 2 (/ (- i 2) 3))))
        (else 1)))

(display "e=")
(+ 2 (count-frac n d 50))