#lang sicp

(define (pascal row col)
  (if (or (<= row 0) (<= col 0) (>= col row))
      1
      (+ (pascal (dec row) col) (pascal (dec row) (dec col)))))

; (pascal 0 0) = 1
; (pascal 1 0) = 1 
; (pascal 1 1) = 1
; (pascal 2 0) = 1
; (pascal 2 1) = 1
; (pascal 2 2) = 1
; (pascal 3 0) = 1
; (pascal 3 1) = 3
; (pascal 3 2) = 3
; (pascal 3 3) = 1
