#lang sicp

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Cannot get front on empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "Cannot delete from empty queue"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (if (empty-queue? queue)
      (begin
        (display "nil")
        (newline))
      (begin
        (display (car (front-ptr queue)))
        (display " ")
        (print-queue (cons (cdr (front-ptr queue))
                           (rear-ptr queue))))))

(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)
; (mcons (mcons 'a '()) '())

(insert-queue! q1 'b)
(print-queue q1)
; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))

(delete-queue! q1)
(print-queue q1)
; (mcons (mcons 'b '()) (mcons 'b '()))

; Ben gets information about internal representation (front, rear ptr)
; of queue, not the actual content of queue elements