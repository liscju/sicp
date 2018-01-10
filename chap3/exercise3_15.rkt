#lang sicp

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1)

; z1 -> [|;->
;        | /
;        |/
;        v
;       ['wow;-> ['b;-> nil

z2
(set-to-wow! z2)


; z2 -> [|;-> ['a;-> ['b;-> nil
;        |
;        v
;       ['wow;-> ['b;-> nil