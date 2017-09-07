#lang sicp

; Retrieves last element of list.
;
; Arguments:
; l - non empty list
;
; Returns:
; last element of list
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))
