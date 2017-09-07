#lang sicp

; Count possible ways of changing amount with specified coins.
;
; Parameters:
; amount - amount to change
; coin-values - values of coins to change amount
;
; Returns:
; number of ways to change amount
(define (cc amount coin-values)
  (let ((no-more? null?)
        (except-first-denomination cdr)
        (first-denomination car))
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))))))

; Order of coin values does not change the result of the algorithm.
; 
