#lang sicp

(list 1 (list 2 (list 3 4)))

; interpreter representation
; (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))

; box-pointer representation
;((1 2) 3 4) -> [][--->[3][--->[4][]
;               |
;               |
;              \|/
;              [1][--->[2][]

; tree representation
;              ((1 2 ) 3  4)
;               /       \  \
;              /         \  \
;             /           3  4
;           (1 2)
;           /  \
;          1    2