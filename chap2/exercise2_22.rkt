#lang sicp

; Returns the value of the argument squared.
;
; Parameters:
; x - value to square
;
; Returns:
; square of the argument
(define (square x)
  (* x x))

; Returned items are in reverse order because (cons (square (card things) answer))
; puts last item on the beggining of the list.
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things))
;                    answer))))
;  (iter items nil))

; Returned items are wrong order because (cons answer (square (car things)))
; puts calculated reversed items as first argument where list in cons should be
; put in first argument.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


