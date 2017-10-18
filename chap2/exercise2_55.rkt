#lang sicp

(car ''abrakadabra)

; 'abrakadabra -> (quote abrakadabra)
; ''abrakadabra -> '(quote abrakadabra) -> (quote (quote abrakadabra))
; (car ''abrakadabra) -> (car (quote (quote abrakadabra)))
; (car (list (quote quote) (quote abrakadabra))) -> (quote quote)