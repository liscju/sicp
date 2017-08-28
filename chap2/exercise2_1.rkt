#lang sicp

; Calculates greatest common disivor of specified numbers.
;
; Examples:
;    (gcd (- 4) (- 2))    == -2
;    (gcd (- 4)    2)     ==  2
;    (gcd    4     2)     ==  2
;    (gcd    8     5)     ==  1
;    (gcd    10    4)     ==  2
;    
; 
;
; Parameters:
; a, b - numbers to calculate greatest common divisor
;
; Returns:
; greatest common divisor of a and b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Returns sign of the specified number.
;
; Parameters:
; x - number to calculate sign
;
; Returns:
; -1 when number less than zero
;  0 when number equals zero
;  1 when number is positive
(define (sign x)
  (if (< x 0)
      (- 1)
      1))

; Constructs rational number from numerator and denominator.
;
; Parameters:
; n - numerator
; d - denominator
;
; Returns:
; rational number with specified nominator and denominator
(define (make-rat n d)
  (let ((nabs (abs n))
        (dabs (abs d)))
    (let ((ndsign (sign (* n d)))
          (g (gcd nabs dabs)))
      (cons (* ndsign (/ nabs g)) (/ dabs g)))))

; Gets numerator of the rational number.
;
; Parameters:
; x - rational number
;
; Returns:
; numberator of the rational number
(define (numer x)
  (car x))

; Gets denominator of the rational number.
;
; Parameters:
; x - rational number
;
; Returns:
; denominator of the rational number
(define (denom x)
  (cdr x))

; Prints rational number on the screen.
;
; Parameters:
; x - rational number to print
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))