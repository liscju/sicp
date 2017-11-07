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
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(equal? (make-sum 0 0) 0)
(equal? (make-sum 0 15) 15)
(equal? (make-sum 15 0) 15)
(equal? (make-sum 15 15) 30)
(equal? (make-sum 0 '(x + 3)) '(x + 3))
(equal? (make-sum '(x + 3) 0) '(x + 3))
(equal? (make-sum 5 '(x + 3)) '(5 + (x + 3)))
(equal? (make-sum '(x + 3) 5) '((x + 3) + 5))
(equal? (make-sum '(x + 3) '(y + 3)) '((x + 3) + (y + 3)))

; Construct multiplication of specified expressions.
;
; Parameters:
; m1 - first expression
; m2 - second expression
;
; Returns:
; constructed multiplication
(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(equal? (make-product 1 1) 1)
(equal? (make-product 1 15) 15)
(equal? (make-product 15 1) 15)
(equal? (make-product 5 5) 25)
(equal? (make-product 1 '(x * 3)) '(x * 3))
(equal? (make-product '(x * 3) 1) '(x * 3))
(equal? (make-product 5 '(x * 3)) '(5 * (x * 3)))
(equal? (make-product '(x * 3) 5) '((x * 3) * 5))
(equal? (make-product '(x * 3) '(y * 3)) '((x * 3) * (y * 3)))

; Checks if expression is a sum.
;
; Parameters:
; x - expression
;
; Returns:
; #t when expression is a sum, #f otherwise
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(equal? (sum? 4) #f)
(equal? (sum? '(x - 4)) #f)
(equal? (sum? '((x + 4) - 5)) #f)
(equal? (sum? '(x + 4)) #t)
(equal? (sum? '(x + (y + 5))) #t)
(equal? (sum? '((x - 4) + 10)) #t)

; Gets first expression added in specified sum.
;
; Parameters:
; s - sum
;
; Returns:
; first expression of a sum
(define (addend s)
  (car s))

(equal? (addend '(2 + 3)) 2)
(equal? (addend '((2 + 3) + 5)) '(2 + 3))
(equal? (addend '(5 + (2 + 3))) 5)
(equal? (addend '((2 + 5) + (3 + 7))) '(2 + 5))

; Gets second expression added in specified sum.
;
; Parameters:
; s - sum
;
; Returns:
; second expression of a sum
(define (augend s)
  (caddr s))

(equal? (augend '(2 + 3)) 3)
(equal? (augend '(2 + (3 - 5))) '(3 - 5))
(equal? (augend '((3 + 5) + 2)) 2)
(equal? (augend '((3 - 5) + (4 + 5))) '(4 + 5))

; Checks if expression is a product.
;
; Parameters:
; x - expression to check
;
; Returns:
; #t when expression is a product, #f otherwise
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(equal? (product? 4) #f)
(equal? (product? '(x - 4)) #f)
(equal? (product? '((x + 4) - 5)) #f)
(equal? (product? '(x * 4)) #t)
(equal? (product? '(x * (y + 5))) #t)
(equal? (product? '((x - 4) * 10)) #t)

; Gets first multiplied expression in product.
;
; Parameters:
; p - product
;
; Returns:
; first multiplied expression in product
(define (multiplier p)
  (car p))

(equal? (multiplier '(2 * 3)) 2)
(equal? (multiplier '((2 + 3) * 5)) '(2 + 3))
(equal? (multiplier '(5 * (2 + 3))) 5)
(equal? (multiplier '((2 + 5) * (3 + 7))) '(2 + 5))

; Gets second multiplied expression in product.
;
; Parameters:
; p - product
;
; Returns:
; second multiplied expression in product
(define (multiplicand p)
  (caddr p))

(equal? (multiplicand '(2 * 3)) 3)
(equal? (multiplicand '((2 + 3) * 5)) 5)
(equal? (multiplicand '(5 * (2 + 3))) '(2 + 3))
(equal? (multiplicand '((2 + 5) * (3 + 7))) '(3 + 7))

; Parses precedences for specified expression.
;
; Parameters:
; exp - expression
;
; Returns:
; expression with parsed precedences
(define (parse-precedence exp) 
  
   (define (simplest-term? exp) 
     (or (variable? exp) (number? exp))) 
  
   (define (build-multiplier-precedence exp) 
     (list (parse-precedence (multiplier exp)) 
           '* 
           (parse-precedence (multiplicand exp)))) 
  
   (define (iterate exp result) 
     (cond ((null? exp) result) 
           ((simplest-term? exp) exp) 
           ((and (> (length exp) 2) (product? exp)) 
            (iterate (cdddr exp) 
                     (cons (build-multiplier-precedence exp) result))) 
           (else 
            (iterate (cdr exp)  
                     (cons (parse-precedence (car exp)) result))))) 
   (iterate exp '())) 

(define (deriv exp var)
  (deriv_ (parse-precedence exp) var))

; Derive expression with respect to variable.
;
; Parameters:
; exp - expression to derive
; var - expression is dervied with respect to variable
;
; Returns:
; derived expression
(define (deriv_ exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv_ (addend exp) var)
                   (deriv_ (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv_ (multiplicand exp) var))
          (make-product (deriv_ (multiplier exp) var)
                        (multiplicand exp))))
        (else (error "Unknown type of expression -- DERIV" exp))))


(equal? (deriv 5 'x) 0)
(equal? (deriv 'x 'x) 1)
(equal? (deriv 'y 'x) 0)

(equal? (deriv '(2 + 3) 'x) 0)
(equal? (deriv '(2 + x) 'x) 1)
(equal? (deriv '(2 + y) 'x) 0)
(equal? (deriv '(x + y) 'x) 1)
(equal? (deriv '(y + x) 'x) 1)
(equal? (deriv '(x + x) 'x) 2)
(equal? (deriv '((5 * x) + (4 * x)) 'x) 9)

(equal? (deriv '(2 * 3) 'x) 0)
(equal? (deriv '(2 * x) 'x) 2)
(equal? (deriv '(2 * y) 'x) 0)
(equal? (deriv '(x * y) 'x) 'y)
(equal? (deriv '(y * x) 'x) 'y)
(equal? (deriv '(x * x) 'x) '(x + x))
(equal? (deriv '((5 + x) * (4 + x)) 'x) '((5 + x) + (4 + x)))

