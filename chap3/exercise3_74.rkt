#lang sicp
(#%require (only racket/stream
                 stream-cons
                 stream-first
                 stream-rest
                 empty-stream
                 stream-empty?
                 stream
                 for/stream
                 stream-map
                 stream-ref
                 stream-filter
                 stream-for-each))

(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define (stream-enumerate-interval start end)
  (if (> start end)
      empty-stream
      (stream-cons start (stream-enumerate-interval (inc start) end))))
(define (display-stream s) (stream-for-each display s))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define ones (stream-cons 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (stream-cons 1 (add-streams ones integers)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (partial-sums s)
  (define partial (stream-cons (stream-car s) (add-streams (stream-cdr s) partial)))
  partial)
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
         ((stream-null? s2) s1)
         (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (stream-cons s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons s2car (merge s1 (stream-cdr s2))))
                  (else
                   (stream-cons s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))

(define (stream-print-first-n s n)
  (if (= n 0)
      (display newline)
      (begin (display (stream-car s))
             (display " ")
             (stream-print-first-n (stream-cdr s) (dec n)))))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-streams s integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) (- 1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (define invert-series
    (stream-cons (stream-car s)
                 (scale-stream (mul-series (stream-cdr s) invert-series) (- 1))))
  invert-series)

(define (square x)
  (* x x))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t w)
  (define (triples-rec s pairz)
    (stream-cons
     (cons (stream-car s) (stream-car pairz))
     (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr pairz))
      (triples-rec (stream-cdr s) (stream-cdr pairz)))))
  (triples-rec s (pairs t w)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
         ((stream-null? s2) s1)
         (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (stream-cons s1car
                                (merge-weighted (stream-cdr s1) s2 weight)))
                  ((> (weight s1car) (weight s2car))
                   (stream-cons s2car
                                (merge-weighted s1 (stream-cdr s2) weight)))
                  (else
                   (stream-cons s1car
                                (merge-weighted (stream-cdr s1)
                                                (stream-cdr s2)
                                                weight))))))))


(define (weighted-pairs s t w)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t))
    w)))

(define integer-pairs-order-by-sum
  (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

(define integers-not-div-by-235
  (stream-filter (lambda (x) (and (not (= (remainder x 2) 0))
                                  (not (= (remainder x 3) 0))
                                  (not (= (remainder x 5) 0))))
                 integers))

(define integers-pairs-not-div-by-235-ordered
  (weighted-pairs integers-not-div-by-235
                  integers-not-div-by-235
                  (lambda (x) (+ (* 2 (car x))
                                 (* 3 (cadr x))
                                 (* 5 (car x) (cadr x))))))

; exercise

(define sense-data sine-series)

(define (sign-change-detector curr prev)
  (cond ((and (>= curr 0) (< prev 0)) 1)
        ((and (< curr 0) (>= prev 0)) (- 1))
        (else 0)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))