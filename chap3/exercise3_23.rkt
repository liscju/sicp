#lang sicp

; queue represented as (cons front rear)
; queue element represented as (cons val (cons prev (cons next nil)))

(define (make-deque)
  (cons '() '()))

(define (front-deque queue)
  (car queue))
(define (rear-deque queue)
  (cdr queue))

(define (set-front-deque! queue item)
  (set-car! queue item))

(define (set-rear-deque! queue item)
  (set-cdr! queue item))

(define (make-queue-element item prev next)
  (cons item (cons prev (cons next))))

(define (set-queue-element-item elem item)
  (set-car! elem item))

(define (get-queue-element-item elem)
  (car elem))

(define (set-queue-element-prev elem prev)
  (set-car! (cdr elem) prev))

(define (get-queue-element-prev elem)
  (cadr elem))

(define (set-queue-element-next elem next)
  (set-car! (cddr elem) next))

(define (get-queue-element-next elem)
  (caddr elem))

(define (front-insert-deque! queue item)
  (let ((new-element (make-queue-element item '() (front-deque queue))))
    (if (null? (front-deque queue))
        (begin (set-front-deque! queue new-element)
               (set-rear-deque! queue new-element))
        (begin (set-queue-element-prev (car (front-deque queue)) new-element)
               (set-front-deque! queue new-element)))))

(define (rear-insert-deque! queue item)
  (let ((new-element (make-queue-element item (rear-deque queue) '())))
    (if (null? (front-deque queue))
        (begin (set-front-deque! queue new-element)
               (set-rear-deque! queue new-element))
        (begin (set-queue-element-next (car (rear-deque queue)) new-element)
               (set-rear-deque! queue new-element)))))

(define (front-delete-deque! queue)
  (if (null? (front-deque queue))
      (error "Cannot delete from empty queue")
      (let ((first-elem (car (front-deque queue))))
        (begin (set-front-deque! queue ())))))

(define (rear-delete-deque! queue))

