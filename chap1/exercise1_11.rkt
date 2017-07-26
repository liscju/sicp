#lang sicp

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (dec n)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

; (f-rec 0) = 0
; (f-rec 1) = 1
; (f-rec 2) = 2
; (f-rec 3) = 4
; (f-rec 4) = 11
; (f-rec 5) = 25

(define (f-iter n)
  (if (< n 3)
      n
      (f-iter-helper 0 1 2 (- n 2))))

(define (f-iter-helper a b c n)
  (if (= n 0)
      c
      (f-iter-helper b c (+ c (* 2 b) (* 3 a)) (dec n))))

; (f-inc 0) = 0
; (f-inc 1) = 1
; (f-inc 2) = 2
; (f-inc 3) = 4
; (f-inc 4) = 11
; (f-inc 5) = 25