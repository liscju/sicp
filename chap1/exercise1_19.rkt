#lang sicp

; T_pq(a, b) = (bq+aq+ap, bp+aq)
; T_pq(bq+aq+ap, bp+aq) = (b(q^2+2*pq) + a(q^2+2*pq) + a(q^2+p^2),
;                          b(q^2+p^2) + a(q^2+2*pq))
; T_pq^2(a, b) = T_{q^2+p^2}{q^2+2*pq}(a,b)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                  b
                  (+ (* q q) (* p p))
                  (+ (* q q) (* 2 p q))
                  (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

