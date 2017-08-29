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


; Point Utilities
; ------------------------------------------------

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


; Rectangle Low Level Utilities - I representation
; ------------------------------------------------

; Constructs rectangle from specfied leftmost down point, width an height.
;
; Parameters:
; ld-point - leftmost down point
; width - width of the rectangle
; height - height of the rectangle
;
; Returns:
; rectangle with given leftmost down point, width and height
;(define (make-rect ld-point width height)
;  (cons ld-point (cons width height)))

; Gets width of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; width of the rectangle
;(define (width-rect r)
;  (car (cdr r)))

; Gets height of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; height of the rectangle
;(define (height-rect r)
;  (cdr (cdr r)))

; Rectangle Low Level Utilities - II representation
; ------------------------------------------------

; Constructs rectangle from specfied diagonal points.
;
; Parameters:
; start-point - first diagonal point
; end-point - second diagonal point
;
; Returns:
; rectangle with specified diagonal
(define (make-rect start-point end-point)
  (cons start-point end-point))

; Gets width of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; width of the rectangle
(define (width-rect r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

; Gets height of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; height of the rectangle
(define (height-rect r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

; Rectangle High Level Utilities
; ------------------------------------------------

; Calculates perimeter of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; perimeter of the rectangle
(define (perimeter-rect r)
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))

; Calculates area of the rectangle.
;
; Parameters:
; r - rectangle
;
; Returns:
; area of the rectangle
(define (area-rect r)
  (* (width-rect r) (height-rect r)))