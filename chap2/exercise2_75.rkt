#lang sicp

(define (square x)
  (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unrecognized operation"))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle a))
          (else (error "Unrecognized operation"))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define z (make-from-mag-ang 10 1.57))
(apply-generic 'imag-part z)
