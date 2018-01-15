#lang sicp

;(define (lookup key-equal? key table)
;  (let ((record (assoc key-equal? key (cdr table))))
;    (if record
;        (cdr record)
;        false)))

(define (assoc key-equal? key records)
  (cond ((null? records) false)
        ((key-equal? key (caar records)) (car records))
        (else (assoc key-equal? key (cdr records)))))

(define (make-table key-equal?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-equal? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-equal? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-equal? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-equal? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr local-table)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Not recognized operation -- TABLE" m))))
    dispatch))

(define delta 0.5)
(define (key-delta-equal? k1 k2)
  (< (abs (- k1 k2)) delta))

(define operation-table (make-table key-delta-equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))






