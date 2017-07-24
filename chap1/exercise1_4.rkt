#lang sicp

(define
  (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; (a-plus-abs-b 3 4) -> ((if (> 4 0) + -) 3 4) ->
; (+ 3 4) -> 7

; (a-plus-abs-b 3 (- 4)) -> ((if (> (- 4) 0) + -) 3 4) ->
; (- 3 (- 4)) -> 7
