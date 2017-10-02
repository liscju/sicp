#lang sicp

; Accumulates elements of sequence using specified operation.
;
; Accumulate operation is defined as follows:
; (accumulate op initial (list e1 e2)) == (op e1 (op e2 initial))
;
; Parameters:
; op - operation that accumulates sequence elements
; initial - initial accumulate value
; sequence - list of elements
;
; Returns:
; accumulated sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Maps sequences in sequence list and appends results in one list.
;
; Parameters:
; proc - procedure to apply on each element of the sequence
; seq - sequence of elements
;
; Returns:
; mapped sequence
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Enumerates integers from start to end inclusive.
;
; Parameters:
; start - first integer
; end - last integer
;
; Returns:
; enumeration of integers from start to end
(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (inc start) end))))

; Generates list of unique pair (i,j).
;
; Each element of generated pair (i,j) belongs to the
; range <1,n>.
;
; Parameters:
; n - maximum value of the element in pair
;
; Returns:
; list of generated pairs
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (dec i))))
           (enumerate-interval 3 n)))

; Generates list of unique triple (i, j).
;
; Each element of the generated triple (i, j, k) belongs
; to the range <1, n>.
;
; Parameters:
; n - maximum value of the element in pair
;
; Returns:
; list of generated triples
(define (unique-triple n)
  (flatmap (lambda (i)
                    (flatmap (lambda (j)
                               (map (lambda (k) (list i j k))
                                    (enumerate-interval 1 (dec j))))
                             (enumerate-interval 2 (dec i))))
                  (enumerate-interval 3 n)))

(equal? (list) (unique-triple 2))
(equal? (list (list 3 2 1)) (unique-triple 3))
(equal? (list (list 3 2 1) (list 4 2 1) (list 4 3 1) (list 4 3 2))
        (unique-triple 4))

; Filters element in the list that satisfied specified predicate.
;
; Parameters:
; p - predicate
; l - list
;
; Returns:
; list containing element that satisfy predicate
(define (filter p l)
  (cond ((null? l) nil)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

; Returns true on any argument.
;
; Parameters:
; x - argument
;
; Returns:
; true
(define (true x)
  #t)

; Returns false on any argument.
;
; Parameters:
; x - argument
;
; Returns:
; false
(define (false x)
  #f)

; Returns true for even number.
;
; Parameters:
; x - number
;
; Returns:
; true when number is even, false otherwise
(define (even x)
  (= (remainder x 2) 0))

; Cases
; empty  : list is empty
(equal? (list) (filter true (list)))
; empty  : predicate always returns false
(equal? (list) (filter false (list 1 2 3)))
; trivial: predicate always returns true
(equal? (list 1 2 3) (filter true (list 1 2 3)))
; normal : predicate filters some numbers
(equal? (list 2 4 6) (filter even (list 2 3 4 5 6)))

; Generates list of all unique triples that each sums to specified value.
;
; Parameters:
; n - maximum value of a number in triple
; sum - expected value of the sum of generated triples
;
; Returns:
; list of all unique triples that each triple sums to specified value 
(define (triple-sums-to-k n sum)
  (filter (lambda (x) (= (accumulate + 0 x) sum)) (unique-triple n)))

(equal? (list) (triple-sums-to-k 2 0))
(equal? (list) (triple-sums-to-k 3 5))
(equal? (list (list 3 2 1)) (triple-sums-to-k 3 6))
(equal? (list (list 4 3 1) (list 5 2 1)) (triple-sums-to-k 5 8))