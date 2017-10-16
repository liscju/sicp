#lang sicp

; Constructs vector with specified coordinates of end point.
;
; Parameters:
; x - first coordinate of vector
; y - second coordinate of vector
;
; Returns:
; vector with end point with specified coordinates
(define (make-vect x y)
  (list x y))

; Gets x coordinate of the specified vector.
;
; Parameters:
; v - vector
;
; Returns:
; x coordinate of the vector
(define (xcor-vect v)
  (car v))

; Gets y coordinate of the specified vector.
;
; Parameters:
; v - vector
;
; Returns:
; y coordinate of the vector
(define (ycor-vect v)
  (car (cdr v)))

; Creates segment from two vectors.
;
; Parameters:
; v1 - vector pointing at the beggining of a segment
; v2 - vector pointing at the end of a segment
;
; Returns:
; segment
(define (make-segment v1 v2)
  (list v1 v2))

; Gets start position of the segment.
;
; Parameters:
; s - segment
;
; Returns:
; start of a segment as vector
(define (start-segment s)
  (car s))

; Gets end position of the segment.
;
; Parameters:
; s - segment
;
; Returns:
; end of a segment as vector
(define (end-segment s)
  (car (cdr s)))