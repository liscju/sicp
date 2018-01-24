#lang sicp

; serialized
;
; because changes happens one after another
; there is no way it can be different than
; (10, 20, 30)

; serialized on single account
;
; (10, 20, 30)
;
; P1 calculates difference 10 between (10, 20)
; P1 withdraw 10 first (0, 20)
; P2 calculates difference 20 between (0, 20)
; P1 deposit 10 second (0, 30)
; P2 withdraw 20 first (-20, 30)
; P2 deposit 20 second (-20, 50)