#lang sicp
(#%require sicp-pict)

; A)

; I use 0.99, 0.02, 0.98 because in my environment there is
; probebly bug in Dr Racket that makes segments invisible otherwise...
(define contour
  (let ((right (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0.01)))
        (top (make-segment  (make-vect 0.02 0.98) (make-vect 0.98 0.98)))
        (down (make-segment (make-vect 1 0) (make-vect 0 0)))
        (left (make-segment (make-vect 0 0) (make-segment 0 1))))
  (segments->painter (list down left right))))

; B)

(define diagonal
  (let ((first (make-segment (make-vect 0 0) (make-vect 1 1)))
        (second (make-segment (make-vect 0 1) (make-vect 1 0))))
    (segments->painter (list first second))))

; C)

(define rhomboid
  (let ((left (make-segment (make-vect 0 0.5) (make-vect 0.5 0.5)))
        (top (make-segment (make-vect 0.5 0.5) (make-vect 1 0.5)))
        (right (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))
        (down (make-segment (make-vect 0.5 0) (make-vect 0 0.5))))
    (segments->painter (list left top right down))))

