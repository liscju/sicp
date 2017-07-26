#lang sicp

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; (+ 4 3)
; (inc (+ 3 3))
; (inc (inc (+ 2 3))
; (inc (inc (inc (+ 1 3))))
; (inc (inc (inc (inc (+ 0 3)))))
; (inc (inc (inc (inc 3))))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7

; recurent process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; (+ 4 3)
; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7

; iterative process