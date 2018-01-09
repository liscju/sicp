#lang sicp

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Balance too small to fulfill request"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Not recognized operation -- MAKE_ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 80))
; Main Env [ acc:dispatch(Acc Env) ]
;             ^
;             |
; Acc Env  [ balance: 80, widthdraw: func, deposit:func]

((acc 'deposit) 40)
; Main Env [ acc:dispatch(Acc Env) ]
;             ^
;             |
; Acc Env  [ balance: 80, widthdraw: func, deposit:func]
;          ^
;          |
;          [ m: 'deposit]
;          ^
;          |
;          [ amount: 40]


((acc 'withdraw) 80)
; Main Env [ acc:dispatch(Acc Env) ]
;             ^
;             |
; Acc Env  [ balance: 40, widthdraw: func, deposit:func]
;          ^
;          |
;          [ m: 'deposit]
;          ^
;          |
;          [ amount: 80]

(define acc2 (make-account 100))

; local states are stored in separate environments
; while make-account is called

; acc and acc2 shares global function make-account