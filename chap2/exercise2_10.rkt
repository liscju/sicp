#lang sicp

; Definition of the interval

; Constructs interval with given lower and upper bound.
;
; Parameters:
; a - lower bound of the interval
; b - upper bound of the interval
;
; Returns:
; intervals with given lower and upper bound
(define (make-interval a b)
  (cons a b))

; Gets lower bound of the interval.
;
; Parameters:
; x - interval
;
; Returns:
; lower bound of the interval
(define (lower-bound x)
  (car x))

; Gets upper bound of the interval.
;
; Parameters:
; x - interval
;
; Returns:
; upper bound of the interval
(define (upper-bound x)
  (cdr x))


; High level interval utilities

; Adds specified intervals.
;
; Parameters:
; x, y - intervals
;
; Returns:
; new interval equals x + y
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Multiplies specified intervals.
;
; Parameters:
; x, y - intervals
;
; Returns:
; new interval equals x * y
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

; Divides specified intervals.
;
; Parameters:
; x, y - intervals
;
; Returns:
; new interval eauals x * y^(-1)
(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "cannot divide by interval having lower or upper bound 0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

; Substracts specified intervals.
;
; Interval substraction is defined as
;
; Parameters:
; x, y - intervals
;
; Returns:
; result of substraction x - y
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

