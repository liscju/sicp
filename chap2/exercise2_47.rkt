#lang sicp

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (orig-frame1 f)
  (car f))

(define (edge1-frame1 f)
  (car (cdr f)))

(define (edge2-frame1 f)
  (car (cdr (cdr f))))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (orig-frame2 f)
  (car f))

(define (edge1-frame2 f)
  (car (cdr f)))

(define (edge2-frame2 f)
  (cdr (cdr f)))
