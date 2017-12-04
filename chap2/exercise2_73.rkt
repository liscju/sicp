#lang sicp

; Primitives used later(put/get)
(define table (list ))
(define (put op type proc)
  (set! table (append table (list (list op type proc)))) 
)
(define (get op type) 
  (define (search op type t) 
   (cond ((null? t) #f) 
    ((and (eqv? (caar t) op) (eqv? (cadar t) type)) 
         (caddar t)) 
    (else (search op type (cdr t))))) 
 (search op type table) 
)
; End of primitives

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

; A)
; Recognizing types in procedure were replaced by
; lookup in table. Procedures number? and variable?
; cannot be delegeted to table because each other
; differentiation is dependent of it - they are
; primitive operations.

; B) C)

(define (install-add)
  (define (deriv-add args var)
    (let ((first (deriv (car args) var))
          (second (deriv (cadr args) var)))
      (cond ((eq? first 0) second)
            ((eq? second 0) first)
            ((and (number? first) (number? second)) (+ first second))
            (else (cons '+ (cons first (cons second nil))))
      )))
  (put 'deriv '+ deriv-add))

(define (install-mul)
  (define (deriv-mul args var)
    (let ((first (car args))
          (first-deriv (deriv (car args) var))
          (second (cadr args))
          (second-deriv (deriv (cadr args) var)))
      (cons '+ (cons (list '* first-deriv second)
                     (cons (list '* first second-deriv) nil)))))
  (put 'deriv '* deriv-mul))


(install-add)
(install-mul)

; D)

; To change get order of arguments to:
; ((get (operator exp) 'deriv) (operands exp) var)
; change order of arguments on each get and put

