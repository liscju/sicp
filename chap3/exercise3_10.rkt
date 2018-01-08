#lang sicp

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "No cash on account"))))

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
(W2 50)

;(define (make-withdraw initial-amount)
;  ((lambda (balance))
;     (let ((balance initial-amount))
;       (lambda (amount)
;         (if (>= balance amount)
;             (begin (set! balance (- balance amount))
;                    balance)
;             "No cash on account"))))
;   initial-amount))

; W1                 [ balance: initial-amount]
; ^                   ^
; |                   |
; |                   |
; lambda (amount) ----/
