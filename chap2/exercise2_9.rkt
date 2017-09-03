#lang sicp

; interval = <x,y>
;
; width(interval) = (y-x)/2
;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;
; width(add(interval1, interval2)) = (y1+y2-x1-x2)/2
; width(add(interval1, interval2)) = (y1-x1)/2 + (y2-x2)/2
; width(add(interval1, interval2)) = width(interval1) + width(interval2)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

; suppose that width on interval multiplication depends only
; on widths of multiplied intervals. That means that for any
; arguments that widths are the same the width of multiplication
; is the same.

; i1 = <1, 2> => width(i1) = 1
; i2 = <3, 4> => width(i2) = 1
;
; width(mul(i1, i2)) = width(<3, 8>) = 2.5
;
; i3 = <3, 4> => width(i3) = 1 => width(i3) = width(i1)
; i4 = <5, 6> => width(i4) = 1 => width(i4) = width(i2)
;
; width(mul(i3, i4)) = width(<15, 24>) = 4.5
;
; width(mul(i1, i2)) != width(mul(i3, i4))
; so contradiction to an assumption 
