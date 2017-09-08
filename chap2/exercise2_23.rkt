#lang sicp

; Performs action on each item on the list.
;
; Arguments:
; action - action to perform
; items - list of item to perform action on
;
; Returns:
; #t
(define (for-each action items)
  (if (null? items)
      #t
      (let ((head (car items))
        (tail (cdr items)))
        (action head)
        (for-each action tail))))
