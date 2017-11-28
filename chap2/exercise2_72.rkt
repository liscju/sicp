#lang sicp

;(define (encode-symbol symbol tree)
;  (cond ((not (contains? (symbols tree) symbol))
;         (error "There is no such an encoding"))
;        ((leaf? tree) nil)
;        ((contains? (symbols (left-branch tree)) symbol)
;         (cons '0 (encode-symbol symbol (left-branch tree))))
;        ((contains? (symbols (right-branch tree)) symbol)
;         (cons '1 (encode-symbol symbol (right-branch tree))))))

; N = 5
; pairs = { (C_{N}, 2^(N-1)), (C_{N-1}, 2^(N-2)), ..., (C_0, 1)}

; In General:
; Smallest symbol encoding in N = > 1
; Highest symbol encoding in N => N

; For smallest
; f(N) = 1

; For largest
; f(N) = O(1) + O(2) + O(3) + ... O(N-1) = O(N^2)