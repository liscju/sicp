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

; Interval constructor/selectors based on center and tolerance.

; Converts specified percentage value to fraction.
;
; Resulted fraction equals percent / 100.
;
; Arguments:
; x - percentage
;
; Returns:
; fraction representing percentage x
(define (percent2fraction x)
  (/ x 100))

; Converts specified fraction to percentage.
;
; Resulted percentage equals fraction * 100.
;
; Arguments:
; x - fraction
;
; Returns:
; percentage representing fraction x
(define (fraction2percent x)
  (* x 100))

; Constructs interval based on given center and tolerance.
;
; Convenient constructor for engineering calculation where
; value of element is usually specified as a certain value
; and tolerance of this value e.g. resistance equals 10 ohm
; with tolerance 10%.
;
; Arguments:
; c - center
; t - tolerance as percent
;
; Returns:
; interval with specified center and tolerance
(define (make-center-percent c t)
  (make-center-width c (* c (percent2fraction t))))

; Gets tolerance of the interval as percent.
;
; Arguments:
; i - interval
;
; Returns:
; tolerance of the interval as percent
(define (percent i)
  (fraction2percent (/ (center i) (width i))))

