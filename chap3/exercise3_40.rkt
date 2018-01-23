#lang sicp

(define x 10)

(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))

; P1 gets x 10 and assign it 100, P2 gets 100 and assign it 100*100*100
; P2 gets x 10 and assigns it 10*10*10, P1 gets 10*10*10 and assign it (10*10*10)^2
; P1 gets first value of x as 10, P2 gets value of x as 10 and assign it
;     10*10*10, P1 gets second value of x as 10*10*10 and assign it
;     10* (10*10*10)
; P2 gets x 10, P1 gets x and assign it 10*10, P2 gets x as 10*10 and
;     P2 gets x as 10*10 and assign it 10*(10*10)*(10*10)
; .... etc

(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda () (s (set! x (* x x))))
 (lambda () (s (set! x (* x x x)))))

; P1 gets x 10 and assign it 100, P2 gets 100 and assign it 100*100*100
; P2 gets x 10 and assigns it 10*10*10, P1 gets 10*10*10 and assign it (10*10*10)^2
