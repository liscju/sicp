#lang sicp

; Primitives used later(put/get)
(define table (list ))
(define (put op type proc)
  (set! table (append table (list (list op type proc)))) 
)
(define (get op type) 
  (define (search op type t) 
   (cond ((null? t) #f) 
    ((and (equal? (caar t) op) (equal? (cadar t) type)) 
         (caddar t)) 
    (else (search op type (cdr t))))) 
 (search op type table) 
)

; Primitives used later (put-coercion/get-coercion)

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))
(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

; begin to change

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad data with tag -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad data with tag -- CONTENTS" datum))))

; end of change

(define (id x) x)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))
          (drop-result (if (or (equal? op 'project) (equal? op 'raise)) id drop)))
      (if proc
          (drop-result (apply proc (map contents args)))
          (if (and (or (<= (length args) 1) (same-all-elements? type-tags)))
              (drop-result (apply apply-generic
                                  (cons op (map raise args))))
              (drop-result (apply apply-generic
                                  (cons op (convert-to-common-type args)))))))))

; Converts argument to common types or returns null
(define (convert-to-common-type args)
  (cond ((<= (length args) 1) args)
        ((= (length args) 2)
         (let ((arg1 (car args))
               (arg2 (cadr args)))
           (let ((arg1type (type-tag arg1))
                 (arg2type (type-tag arg2)))
             (cond ((is-subtype? arg1 arg2type)
                    (list (raise-to-type arg1 arg2type) arg2))
                   ((is-subtype? arg2 arg1type)
                    (list arg1 (raise-to-type arg2 arg1type)))
                   (else (error "Arguments are not subtypes" arg1type arg2type))))))
         (error
          "length more than 2")))

(define (same-all-elements? elements)
  (define (all-elements-equals? element elements)
    (if (null? elements)
        #t
        (let ((elements-first (car elements))
              (elements-rest (cdr elements)))
          (if (not (equal? element elements-first))
              #f
              (all-elements-equals? element elements-rest)))))
  (if (null? elements)
      #t
      (all-elements-equals? (car elements) (cdr elements))))

; (same-all-elements? (list))
; (same-all-elements? (list 1))
; (same-all-elements? (list 1 1))
; (not (same-all-elements? (list 1 2)))

; End of primitives

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atane x) (apply-generic 'atane x))
(define (rounde x) (apply-generic 'rounde x))
(define (=zero? x) (apply-generic '=zero? x))
(define (opposite x) (apply-generic 'opposite x))
(define (greatest-common-divisor a b) (apply-generic 'gcd a b))

(define (is-subtype? arg type)
  (let ((arg-type (type-tag arg)))
    (if (equal? arg-type type)
      #t
      (let ((raise-proc (get 'raise (list arg-type))))
        (if raise-proc
            (is-subtype? (apply-generic 'raise arg) type)
            #f)))))

(define (raise-to-type arg type)
  (let ((arg-type (type-tag arg)))
    (if (equal? arg-type type)
        arg
        (if (is-subtype? arg type)
            (raise-to-type (raise arg) type)
            (error "type cannot be raised to type")))))

(define (drop x)
  (if (not (pair? x))
      x
      (let ((project-x (project x)))
        (cond ((equal? (type-tag project-x) (type-tag x)) x)
              ((equal? (raise project-x) x) (drop project-x))
              (else x)))))

; complex numbers

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (mul (real-part z) (real-part z))
             (mul (imag-part z) (imag-part z)))))
  (define (angle z)
    (atane (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cosine a)) (* r (sine a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2) (and (equal? (real-part z1) (real-part z2))
                            (equal? (imag-part z1) (imag-part z2)))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cons (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (add (mul x x) (mul y y)))
          (atane y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(polar polar)
       (lambda (z1 z2) (and (equal? (magnitude z1) (magnitude z2))
                            (equal? (angle z1) (angle z2)))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (equal? (real-part z1) (real-part z2))
                            (equal? (imag-part z1) (imag-part z2)))))
  (put 'zero? '(complex)
       (lambda (z) (and (equal? (real-part z) 0) (equal? (imag-part z) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project '(complex)
       (lambda (z) (make-rational (rounde (real-part z)) 1)))
  (put '=zero? '(complex)
       (lambda (z) (and (=zero? (real-part z))
                        (=zero? (imag-part z)))))
  (put 'opposite '(complex)
       (lambda (z) (tag (make-from-real-imag
                         (opposite (real-part z))
                         (opposite (imag-part z))))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; rational numbers

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put 'zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  
  (put 'raise '(rational)
       (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
  (put 'project '(rational)
       (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))
  
  (put 'sine '(rational)
       (lambda (x) (sin (round (div (numer x) (denom x))))))
  (put 'cosine '(rational)
       (lambda (x) (cos (round (div (numer x) (denom x))))))
  (put 'atane '(rational rational)
       (lambda (x y) (atan (round (div (numer x) (denom x)))
                           (round (div (numer y) (denom y))))))
  (put 'rounde '(rational)
       (lambda (x) (round (div (numer x) (denom x)))))
  (put 'opposite '(rational)
       (lambda (x) (make-rational (- (numer x)) (denom x))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; scheme numbers

(define (install-scheme-number-package)
  (define (gcd-numbers a b)
    (if (= b 0)
        a
        (gcd-numbers b (remainder a b))))
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'project '(scheme-number)
       (lambda (x) x))
  (put 'sine '(scheme-number)
       (lambda (x) (sin x)))
  (put 'cosine '(scheme-number)
       (lambda (x) (cos x)))
  (put 'atane '(scheme-number scheme-number)
       (lambda (x y) (atan x y)))
  (put 'rounde '(scheme-number)
       (lambda (x) (round x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'opposite '(scheme-number)
       (lambda (x) (- x)))
  (put 'gcd '(scheme-number scheme-number)
       (lambda (a b) (tag (gcd-numbers a b))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; polynomials

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; terms (rare polynomial) implementation
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; end of terms implementation

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Poly contains different variables -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (opposite-terms L)
    (mul-term-by-all-terms (make-term 0 (- 1)) L))
  (define (sub-terms L1 L2)
    (add-terms L1 (opposite-terms L2)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Poly contains different variables -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((terms-and-rest
               (div-terms (term-list p1)
                          (term-list p2))
               ))
          (let ((terms (car terms-and-rest))
                (rest (cadr terms-and-rest)))
            (list (variable p1) terms rest)))
        (error "Poly contains different variables -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-t (make-term new-o new-c)))
                  (let ((rest-of-result
                         (div-terms (sub-terms L1 (mul-term-by-all-terms new-t L2)) L2)))
                    (let ((rest-result-terms (car rest-of-result))
                          (rest-result-rest (cadr rest-of-result)))
                      (list (cons new-t rest-result-terms) rest-result-rest)))))))))  
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (pseudoremainder-terms a b)
    (let ((o1 (order (first-term a)))
          (o2 (order (first-term b)))
          (c (coeff (first-term b))))
      (let ((normalized-term (exp c (+ 1 o1 (- o2)))))
        (let ((normalized-a (mul-term-by-all-terms (list 0 normalized-term) a)))
          (cadr (div-terms normalized-a b))))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (let* ((coeff-list (map cadr a))
               (gcd-coeff (apply gcd coeff-list)))
          (div-terms a (list (make-term 0 gcd-coeff))))
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (gcd-terms (term-list p1)
                                    (term-list p2)))
        (error "Poly contains different variables -- GCD-POLY"
               (list p1 p2))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (opposite p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'opposite '(polynomial)
       (lambda (p) (make-polynomial (variable p)
                                    (mul-term-by-all-terms
                                     (make-term 0 (- 1))
                                     (term-list p)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'project '(polynomial)
       (lambda (p) (tag p)))
  (put 'raise '(polynomial)
       (lambda (p) (tag p)))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (gcd-poly p1 p2)))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; install packages

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

; conversion

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y)
  (apply-generic 'exp x y))

(define (scheme-number->complex n)
  (make-complex-from-real-imag n 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

; multi argument conversion

;(add
; (make-rational 40 1)
; (make-complex-from-real-imag 5 0))

;(not (=zero? (make-scheme-number 45)))
;(=zero? (make-scheme-number 0))

;(not (=zero? (make-rational 40 1)))
;(=zero? (make-rational 0 1))

;(not (=zero? (make-complex-from-real-imag 0 10)))
;(not (=zero? (make-complex-from-real-imag 10 0)))
;(=zero? (make-complex-from-real-imag 0 0))

;(equal? (opposite (make-complex-from-real-imag 20 10))
;        (make-complex-from-real-imag (- 20) (- 10)))
;(equal? (opposite (make-rational 40 1)) (make-scheme-number (- 40)))
;(equal? (opposite (make-scheme-number 20)) (make-scheme-number (- 20)))

;(define p1
;  (make-polynomial 'x (list (list 2 1) (list 1 2) (list 0 1))))

;(equal? (opposite p1)
;        (make-polynomial 'x (list (list 2 (- 1)) (list 1 (- 2)) (list 0 (- 1)))))

;(define p1 (make-polynomial 'x (list 8 9 7 1 2)))
;(define p2 (make-polynomial 'x (list     4 6 4)))
;(equal? (add p1 p2) (make-polynomial 'x (list 8 9 11 7 6)))

;(display (div-terms
;   (list (list 5 1) (list 0 (- 1)))
;   (list (list 2 1) (list 0 (- 1)))))

;(define p1 (make-polynomial 'x (list (list 5 1) (list 0 (- 1)))))
;(define p2 (make-polynomial 'x (list (list 2 1) (list 0 (- 1)))))

;(equal? (div p1 p2)
;        (make-polynomial 'x
;                         (list (list (list 3 1) (list 1 1))
;                               (list (list 1 1) (list 0 (- 1))))))


;> rf
;(mcons
; 'rational
; (mcons
;  (mcons
;   'polynomial
;   (mcons 'x (mcons (mcons 3 (mcons 1 '())) (mcons (mcons 0 (mcons 1 '())) '()))))
;  (mcons
;   'polynomial
;   (mcons 'x (mcons (mcons 2 (mcons 1 '())) (mcons (mcons 0 (mcons 1 '())) '()))))))

;(add rf rf)

;(mcons
; 'rational
; (mcons
;  (mcons
;   'polynomial
;   (mcons
;    'x
;    (mcons
;     (mcons 5 (mcons 2 '()))
;     (mcons
;      (mcons 3 (mcons 2 '()))
;      (mcons (mcons 2 (mcons 2 '())) (mcons (mcons 0 (mcons 2 '())) '()))))))
;  (mcons
;   'polynomial
;   (mcons
;    'x
;    (mcons
;     (mcons 4 (mcons 1 '()))
;     (mcons (mcons 2 (mcons 2 '())) (mcons (mcons 0 (mcons 1 '())) '())))))))

;(gcd 5 3)

; greatest common divisor
; (define p1 (make-polynomial 'x (list (list 5 1) (list 0 (- 1)))))
; (define p2 (make-polynomial 'x (list (list 2 1) (list 0 (- 1)))))
; (equal? (greatest-common-divisor p1 p2) (make-polynomial 'x (list (list 1 1) (list 0 (- 1)))))

(define p1 (make-polynomial 'x (list (list 2 1) (list 1 (- 2)) (list 0 1))))
(define p2 (make-polynomial 'x (list (list 2 11) (list 0 7))))
(define p3 (make-polynomial 'x (list (list 1 13) (list 0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
