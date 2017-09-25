#lang sicp

; Accumulates elements of sequence using specified operation.
;
; Accumulate operation is defined as follows:
; (accumulate op initial (list e1 e2)) == (op e1 (op e2 initial))
;
; Parameters:
; op - operation that accumulates sequence elements
; initial - initial accumulate value
; sequence - list of elements
;
; Returns:
; accumulated sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Another name for accumulate.
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

; Accumulates elements of sequence from left side.
;
; Accumulate left is defined as follows:
; (fold-left op initial (list e1 e2 e3 ...)) =
;         (op (op (op initial e1) e2) e3 ...)
;
; Parameters:
; op - accumulate operator
; initial - initial value of accumulator
; sequence - sequence to accumulate
;
; Returns:
; accumulated value
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Appends element at the end of list.
;
; Parameters:
; e - element to add
; l - list
(define (append-el e l)
  (if (null? l)
      (cons e nil)
      (cons (car l) (append-el e (cdr l)))))

; Reverses a sequence using fold-right.
;
; Parameters:
; sequence - sequence to reverse
;
; Returns:
; reversed sequence
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append-el x y)) nil sequence))

(equal? (reverse-right (list 1 2 3))
        (list 3 2 1))

; Reverses a sequence using fold-left.
;
; Parameters:
; sequence - sequence to reverse
;
; Returns:
; reversed sequence
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(equal? (reverse-left (list 1 2 3))
        (list 3 2 1))








