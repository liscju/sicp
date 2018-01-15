#lang sicp

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Not recognized operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (lookup key table) ((table 'lookup-proc) key))
(define (insert! key value table) ((table 'insert-proc!) key value))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (memo-fib (- n 1))
                          (memo-fib (- n 2))))))))


; (memo-fib)       (memo-fib 3)
; |                  |
; |                  V
; |                  [x: 3]
; |                  |
; V                  V
; {lambda in memoize, env}
;                     |
;                     V
;                     [table: {make-table...}]
;                     |
;                     V
;                     [f: {lambda (n) (cond...), }]
;                     |
;                     V
;                    [global env]

; (memo-fib n) calculates in the count of step proportional to n because
; each step is calculated only once, that is:
;
; 0(n) = n* O(single step)

; If we define memo-fib using (memoize fib) it would not work because
; fib internally executes itselves (fib (- n 1)..), and those invocations
; are not memoized