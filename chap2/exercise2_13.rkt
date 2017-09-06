#lang sicp

; Multiplies specified intervals.
;
; Parameters:
; x, y - intervals
;
; Returns:
; new interval equals x * y
;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                 (max p1 p2 p3 p4))))

; tolerance = width(i)/center(i)

; Suppose all interval edges are positive:
;
; c1 - center for interval 1
; interval1 = <c1-d1,c1+d1>
; tolerance1 = d1/c1
;
; interval2 = <c2-d2,c2+d2>
; tolerance2 = d2/c2
;
; (mul-interval i1 i2)
; -> (make-interval (* (lower-bound i1) (lower-bound i2)) (* (upper-bound i1) (upper-bound i2)))
; -> (make-interval (* (- c1 d1) (- c2 d2)) (* (+ c1 d1) (+ c2 d2)))
; interval resulted = <(c1-d1)*(c2-d2);(c1+d1)*(c2+d2)>


; width resulted = {(c1+d1)*(c2+d2)-(c1-d1)*(c2-d2)}/2
; = {(c1*c2+c1*d2+d1*c2+d1*d2)-(c1*c2-c1*d2-d1*c2+d1*d2)}/2
; = {c1*c2+c1*d2+d1*c2+d1*d2-c1*c2+c1*d2+d1*c2-d1*d2}/2
; = {c1*d2+d1*c2+c1*d2+d1*c2}/2
; = {2*c1*d2+2*c2*d1}/2
; = c1*d2+c2*d1

; center resulted =  {(c1+d1)*(c2+d2)+(c1-d1)*(c2-d2)}/2
; = {c1*c2+c1*d2+d1*c2+d1*d2+c1*c2-c1*d2-d1*c2+d1*d2}/2
; = {c1*c2+d1*d2+c1*c2+d1*d2}/2
; = {2*c1*c2+2*d1*d2}/2
; = c1*c2 + d1*d2

; tolerance resulted = width/center
; = (c1*d2+c2*d1)/(c1*c2 + d1*d2)
; d1 suppose to be small, d2 suppose to be small so d1*d2 can be omitted
; = (c1*d2+c2*d1)/(c1*c2)
; = (d2/c2) + (d1/c1)
; = tolerance2 + tolerance1


