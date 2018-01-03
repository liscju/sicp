#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((equal? x 'reset-count) (set! count 0))
            ((equal? x 'how-many-calls?) count)
            (else
             (begin (set! count (inc count))
                    (f x)))))))

(define a (make-monitored sqrt))

(a 100)

(a 'how-many-calls?)

(a 200)

(a 'how-many-calls?)

(a 'reset-count)

(a 'how-many-calls?)
