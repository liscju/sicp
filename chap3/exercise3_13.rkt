#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle z)
  (set-cdr! (last-pair z) z)
  z)

(define z (make-cycle (list 'a 'b 'c)))

;    --------------
;   /              \
;   |               \
;   v                |
; ['a|->['b|->['c|->/

; (last-pair z) <- forever loop