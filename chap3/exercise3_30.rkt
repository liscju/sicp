#lang sicp

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c)
  (if (null? (cdr as))
      'ok
      (let ((out-c (make-wire)))
        (full-adder (car as) (car bs) c (car ss) out-c)
        (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) out-c))))
