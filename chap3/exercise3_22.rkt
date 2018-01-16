#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
      (define (set-front-ptr! item)
        (set! front-ptr item))
      (define (set-rear-ptr! item)
        (set! rear-ptr item))
      
      (define (empty-queue?)
        (null? front-ptr))
      
      (define (front-queue)
        (if (empty-queue?)
            (error "Cannot get front on empty queue")
            (car front-ptr)))
      
      (define (insert-queue! item)
        (let ((new-pair (cons item '())))
          (cond ((empty-queue?)
                 (set-front-ptr! new-pair)
                 (set-rear-ptr! new-pair)
                 (cons front-ptr rear-ptr))
                (else
                 (set-cdr! rear-ptr new-pair)
                 (set-rear-ptr! new-pair)
                 (cons front-ptr rear-ptr)))))
      
      (define (delete-queue!)
        (cond ((empty-queue?)
               (error "Cannot delete from empty queue"))
              (else
               (set-front-ptr! (cdr front-ptr))
               (cons front-ptr rear-ptr))))
      
      (define (print-queue)
        (define (print-list l)
          (if (null? l)
              (begin (display "nil")
                     (newline))
              (begin (display (car l))
                     (display " ")
                     (print-list (cdr l)))))
        (print-list front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Operation not applicable -- COMB" m))))
    dispatch))

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item))
(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (print-queue queue) ((queue 'print-queue)))

(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)