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

; Version of accumulates that works on sequence of sequences.
;
; Each sequence in sequences is expected to have same number
; of elements.
;
; Parameters:
; op - operation that accumulates nested sequence elements
; init - initial nested accumulates values
; segs - sequence of sequences
(define (accumulate-n op init segs)
  (if (null? (car segs))
      nil
      (cons (accumulate op init (map car segs))
            (accumulate-n op init (map cdr segs)))))

;(equal? (list) (accumulate-n + 0 (list)))
(equal? nil (accumulate-n + 0 (list (list))))
(equal? (list 1 2 2) (accumulate-n + 0 (list (list 1 2 2))))
(equal? (list 6 8) (accumulate-n + 0 (list (list 1 2) (list 5 6))))