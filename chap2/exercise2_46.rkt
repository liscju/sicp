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

; Adds two vectors.
;
; Result of adding two vectors <x1, y1> and <x2, y2> is defined
; as <x1+x2, y1+y2>
;
; Parameters:
; v1 - first vector
; v2 - second vector
;
; Return:
; result of adding vectors
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(equal? (make-vect 0 0) (add-vect (make-vect 0 0) (make-vect 0 0)))
(equal? (make-vect 0 0) (add-vect (make-vect (- 1) (- 1)) (make-vect 1 1)))
(equal? (make-vect 4 6) (add-vect (make-vect 1 2) (make-vect 3 4)))

; Substracts two vectors.
;
; Result of substracting two vectors <x1, y1> and <x2, y2> is defined
; as <x1 - x2, y1 - y2>
;
; Parameters:
; v1 - first vector
; v2 - second vector
;
; Returns:
; result of substracting vectors
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(equal? (make-vect 0 0) (sub-vect (make-vect 0 0) (make-vect 0 0)))
(equal? (make-vect 0 0) (sub-vect (make-vect 1 1) (make-vect 1 1)))
(equal? (make-vect 2 3) (sub-vect (make-vect 4 6) (make-vect 2 3)))

; Multiplies vector by a scalar.
;
; Result of multiplication of scalar s by vecttor <x1, y1> is defined
; as <s*x1, s*y1>.
;
; Parameters:
; s - scalar
; v - vector
;
; Returns:
; result of vector multiplication by a scalar
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(equal? (make-vect 0 0) (scale-vect 1 (make-vect 0 0)))
(equal? (make-vect 0 0) (scale-vect 0 (make-vect 3 4)))
(equal? (make-vect 4 6) (scale-vect 2 (make-vect 2 3)))