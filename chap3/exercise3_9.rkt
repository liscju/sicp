#lang sicp

(define (factorial-rec n)
  (if (= n 1)
      1
      (* n (factorial-rec (- n 1)))))


; []        :
;    (factorial-rec 6)
; [ n=6 ]   :
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    (* 6 (factorial-rec 5))
; [ n=6 ] <- [ n=5 ]
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    (* 5 (factorial-rec 4))
; [ n=6 ] <- [ n=5 ] <- [ n=4 ]
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    (* 4 (factorial-rec 3))
; [ n=6 ] <- [ n=5 ] <- [ n=4 ] <- [ n=3 ]
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    (* 3 (factorial-rec 2))
; [ n=6 ] <- [ n=5 ] <- [ n=4 ] <- [ n=3 ] <- [ n=2 ]
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    (* 2 (factorial-rec 1))
; [ n=6 ] <- [ n=5 ] <- [ n=4 ] <- [ n=3 ] <- [ n=2 ] <- [ n=1 ]
;    (if (= n 1) 1 (* n (factorial-rec (- n 1))))
;    1


; -----------------------------


(define (factorial-iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))


; (factorial-iter 6)
; [ n=6 ]    :
;    (fact-iter 1 1 n)
; [ n=6 ] <- [ product=1, counter=1, max_count=6 ] :
;    ((if (> counter max-count) product (fact-iter (* counter product) (+ counter 1) max-count)))
;    (fact-iter 1 2 6)
; [ n=6 ] <- [ product=1, counter=1, max_count=6 ] <- [ product=1, counter=2, max_count=6 ]
; ....









