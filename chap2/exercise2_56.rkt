#lang sicp

; Accumulates elements of sequence using specified operation.
;
; Accumulate operation is defined as follows:
; (accumulate op initial (list e1 e2)) == (op e1 (op e2 initial))
;
; Parameters:
; op - operation that accumulates sequence elements
; initial - initial accumulate value
; sequence - list of elements
;
; Returns:
; accumulated sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Checks if argument is a variable.
;
; Parameters:
; x - argument to check
;
; Returns:
; #t when x is a variable, #f otherwise
(define (variable? x) (symbol? x))

; Checks if specified variables are same.
;
; Parameters:
; v1 - first variable
; v2 - second variable
;
; Returns:
; #t when variables are same, #f otherwise
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Constructs sum of specified expressions.
;
; Parameters:
; a1 - first expression
; a2 - second expression
;
; Returns:
; constructed sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

; Checks if expression is equal specified number.
;
; Parameters:
; exp - expression to check
; num - number
;
; Returns:
; #t when expression is equal number, #f otherwise
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Construct multiplication of specified expressions.
;
; Parameters:
; m1 - first expression
; m2 - second expression
;
; Returns:
; constructed multiplication
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Checks if expression is a sum.
;
; Parameters:
; x - expression
;
; Returns:
; #t when expression is a sum, #f otherwise
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; Gets first expression added in specified sum.
;
; Parameters:
; s - sum
;
; Returns:
; first expression of a sum
(define (addend s)
  (cadr s))

; Gets second expression added in specified sum.
;
; Parameters:
; s - sum
;
; Returns:
; second expression of a sum
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (accumulate make-sum 0 (cddr s))))

(equal? (augend '(+ x 10)) 10)
(equal? (augend '(+ x (+ x 15))) '(+ x 15))
(equal? (augend '(+ x x 15)) '(+ x 15))
(equal? (augend '(+ x x x x 15)) '(+ x (+ x (+ x 15))))

; Checks if expression is a product.
;
; Parameters:
; x - expression to check
;
; Returns:
; #t when expression is a product, #f otherwise
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; Gets first multiplied expression in product.
;
; Parameters:
; p - product
;
; Returns:
; first multiplied expression in product
(define (multiplier p)
  (cadr p))

; Gets second multiplied expression in product.
;
; Parameters:
; p - product
;
; Returns:
; second multiplied expression in product
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (accumulate make-product 1 (cddr p))))

(equal? (multiplicand '(* x 10)) 10)
(equal? (multiplicand '(* x (+ x 15))) '(+ x 15))
(equal? (multiplicand '(* x x 15)) '(* x 15))
(equal? (multiplicand '(* x x x x 15)) '(* x (* x (* x 15))))

; Constructs exponentiation with base to the specified power.
;
; Parameters:
; base - the base
; power - the exponent 
(define (make-exponentiation base power)
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        (else (list '** base power))))

; Checks if specified expression is exponentiation.
;
; Parameters:
; e - expression
;
; Returns:
; #t when expression is exponentiation, #f otherwise
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

; Gets base of specified exponentiation.
;
; Parameters:
; e - exponentiation
;
; Returns:
; base of exponentiation
(define (base e)
  (cadr e))

; Gets exponent of specified exponentiation.
;
; Parameters:
; e - exponentiation
;
; Returns:
; exponent of exponentiation
(define (exponent e)
  (caddr e))

; Derive expression with respect to variable.
;
; Parameters:
; exp - expression to derive
; var - expression is dervied with respect to variable
;
; Returns:
; derived expression
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation
                                      (base exp)
                                      (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else (error "Unknown type of expression -- DERIV" exp))))

(equal? (deriv '(** x 1) 'x) 1)
(equal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))
(equal? (deriv '(+ (+ x 10) x) 'x) 2)
(equal? (deriv '(+ (* x 10) x) 'x) 11)