#lang sicp

; Identity function
;
; (id x) == x
;
; Parameters:
; x - argument
;
; Returns:
; same value as argument passed
(define (id x)
  x)

; Filters element from sequence that satisfy specified predicate.
;
; Parameters:
; predicate - unary boolean function
; sequence - list which elements are filtered
;
; Returns:
; filtered elements from sequence
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

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

; Transforms elements of a sequence using specified function.
;
; Parameters:
; p - function transforming elements
; sequence - list of elements
;
; Returns:
; list of elements from sequence transformed with specified function
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(equal? (map inc nil) nil)
(equal? (map id (list 1)) (list 1))
(equal? (map inc (list 1)) (list 2))
(equal? (map inc (list 1 2)) (list 2 3))

; Appends elements from first sequence to second sequence.
;
; Parameters:
; seq1 - first sequence
; seq2 - second sequence
;
; Parameters:
; sequence consisting of elements from first sequence added to
; second sequence
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(equal? (append nil nil) nil)
(equal? (append (list 1 2) nil) (list 1 2))
(equal? (append nil (list 1 2)) (list 1 2))
(equal? (append (list 1 2) (list 3 4)) (list 1 2 3 4))

; Calculates number of elements in the sequence.
;
; Parameters:
; sequence
;
; Returns:
; number of elements in the sequence
(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(equal? (length nil) 0)
(equal? (length (list 1)) 1)
(equal? (length (list 1 2)) 2)