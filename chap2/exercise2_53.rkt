#lang sicp

; Returns list subset that starts with specified item.
;
; Parameters:
; item - symbol to search for
; x - list of symbols
;
; list subset that starts with specified item, otherwise #f
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)

; (cons 'a (cons 'b (cons 'c '())))

(list (list 'george))

; (cons (cons 'george '()) '())

(cdr '((x1 x2) (y1 y2)))

; (cons (cons 'y1 (cons 'y2 '())) '())

(cadr '((x1 x2) (y1 y2)))

; (cons 'y1 (cons 'y2 '()))

(pair? (car '(a short list)))

; #f

(memq 'red '((red shoes) (blue socks)))

; #f

(memq 'red '(red shoes blue socks))

; (cons 'red (cons 'shoes (cons 'blue (cons 'socks '()))))

