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

; Calculates tangens value with Lambert approximation for argument 'x' and
; specified depth.
;
; Lambert tangent approximation is the following fraction:
;
; tan(x) =        x
;            ----------------------
;              1 -   x * x
;                  ----------------
;                    3 -   x * x
;                        ----------
;                          5 - ....
;
; Parameters:
; x     - calculate tangens at this point
; depth - depth of the fraction (bigger values leads to better precision)
;
; Returns:
; value of tangens approximation
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* 2 i) 1))
  (count-frac n d k))