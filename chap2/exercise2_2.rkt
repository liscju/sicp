#lang sicp

; Calculates average value of two numbers.
;
; Parameters:
; x,y - numbers
;
; Returns:
; average value of specified numbers
(define (average x y)
  (/ (+ x y) 2))

; Constructs a point with specified x-y coordinate.
;
; Parameters:
; x - X-coordinate of the point
; y - Y-coordinate of the point
;
; Returns:
; point with specified X-Y coordinates
(define (make-point x y)
  (cons x y))

; Gets X-coordinate of the point.
;
; Parameters:
; p - point
;
; Returns:
; X-coordinate of the point
(define (x-point p)
  (car p))

; Gets Y-coordinate of the point.
;
; Parameters:
; p - point
;
; Returns:
; Y-coordinate of the point
(define (y-point p)
  (cdr p))

; Prints point on the screen.
;
; Parameters:
; p - point to print
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Constructs segment from two points.
;
; Parameters:
; start-point - first point of the segment
; end-point - second point of the segment
;
; Returns:
; segment with given start and end point
(define (make-segment start-point end-point)
  (cons start-point end-point))

; Gets start point of the segment.
;
; Parameters:
; s - segment
;
; Returns:
; start point of the segment
(define (start-segment s)
  (car s))

; Gets end point of the segment.
;
; Parameters:
; s - segment
;
; Returns:
; end point of the segment
(define (end-segment s)
  (cdr s))

; Calculates midpoint point of the segment.
;
; Parameters:
; s - segment
;
; Returns:
; point that is midpoint of the segment
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

; Prints segment on the screen.
;
; Parameters:
; s- segment
(define (print-segment s)
  (print-point (start-segment s))
  (display "->")
  (print-point (end-segment s)))
