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

; Interval constructor/selectors based on center and width

; Constructs interval with given center and width.
;
; Arguments:
; c - center
; w - width
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; Gets center of the specified interval.
;
; Arguments:
; i - interval
;
; Returns:
; center of the interval
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; Gets width of the specified interval.
;
; Arguments:
; i - interval
;
; Returns:
; returns width of the interval
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;> (define A (make-center-width 15000 15))
;> (define B (make-center-width 5000 5))
;> (div-interval A A)
;(mcons 0.998001998001998 1.002002002002002)
;> (define A (make-center-width 15000000 15))
;> (div-interval A A)
;(mcons 0.999998000002 1.000002000002)
