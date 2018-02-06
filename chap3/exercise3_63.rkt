#lang sicp

; Less effective because it calculates (sqrt-stream x)
; once again instead of using calculated values,
; the number of sqrt-stream function call grows
; expotentially
