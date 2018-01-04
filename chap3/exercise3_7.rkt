#lang sicp

(define (make-account password balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Not enough money")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'state) (lambda () balance))
          (else (error "Unrecognized operation -- MAKE-ACCOUNT" m))))
  (lambda (pass msg)
    (if (eq? pass password)
        (dispatch msg)
        (error "Password not valid"))))

(define (make-joint accnt accnt-pass new-pass)
  (lambda (pass msg)
    (if (eq? pass new-pass)
        (accnt accnt-pass msg)
        (error "Password not valid for new account"))))

(define paul-acc (make-account 'open-sesame 100))
(define peter-acc (make-joint paul-acc 'open-sesame 'new-sesame))

; > ((paul-acc 'open-sesame 'state))
; 100
; > ((peter-acc 'new-sesame 'state))
; 100
; > ((paul-acc 'open-sesame 'withdraw) 50)
; 50
; > ((peter-acc 'new-sesame 'state))
; 50
; > ((paul-acc 'open-sesame 'state))
; 50
; > 