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
(define (count-frac-rec n d k)
  (define (count-frac-rec-work i)
    (cond ((> i k) 0)
           ((= i k) (/ (n i)
                       (d i)))
           ((< i k) (/ (n i)
                       (+ (d i) (count-frac-rec-work (inc i)))))))
  (count-frac-rec-work 1))

(count-frac-rec (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

; Expected 1/golder_ratio = 0.6180
; Actual 1/golder_ratio (after 11 iterations) = 0.6180 

; Same as count-frac-rec but it generates iteration process.
(define (count-frac-iter n d k)
  (define (count-frac-iter-work i val)
    (cond ((= i 0) val)
          (else (count-frac-iter-work (dec i)
                                      (/ (n i) (+ (d i) val))))))
  (count-frac-iter-work k 0))

(count-frac-iter (lambda (i) 1.0)
                 (lambda (i) 1.0)
                  11)