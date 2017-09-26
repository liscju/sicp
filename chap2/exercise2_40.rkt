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

(define (doublelist l) (list l l))
(equal? (list) (flatmap doublelist (list)))
(equal? (list (list 1 2) (list 1 2))
        (flatmap doublelist (list (list 1 2))))
(equal? (list (list 1 2) (list 1 2) (list 3 4) (list 3 4))
        (flatmap doublelist (list (list 1 2)
                                  (list 3 4))))

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

(equal? (list) (enumerate-interval 1 0))
(equal? (list 1) (enumerate-interval 1 1))
(equal? (list 1 2) (enumerate-interval 1 2))

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
           (enumerate-interval 2 n)))

(equal? (list) (unique-pairs 0))
(equal? (list) (unique-pairs 1))
(equal? (list (list 2 1)) (unique-pairs 2))
(equal? (list (list 2 1) (list 3 1) (list 3 2)) (unique-pairs 3))

; Return true if any element on the list is true.
;
; Parameters:
; l - list
;
; Returns:
; true when any element on the list is true, false otherwise
(define (any l)
  (cond ((null? l) #f)
        ((car l) #t)
        (else (any (cdr l)))))

(not (any (list)))
(any (list #t))
(not (any (list #f)))
(any (list #f #f #t))

; Returns true if specified number is a prime.
;
; Parameters:
; number - number to check
;
; Returns:
; true if number is prime, false otherwise
(define (prime? number)
  (define (dividesNumber? i) (= 0 (remainder number i)))
  (not (any (map dividesNumber? (enumerate-interval 2 (dec number))))))

(prime? 2)
(prime? 3)
(not (prime? 4))
(prime? 5)
(not (prime? 6))
(prime? 7)

; Checks if sum of specified pair is prime number.
;
; Parameters:
; pair - two elements pair
;
; Returns:
; true if sum is prime, false otherwise
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(prime-sum? (list 1 1))
(prime-sum? (list 2 1))
(not (prime-sum? (list 3 1)))

; Creates triple of specified two elements and its sum.
;
; Parameters:
; pair - two elements in pair
;
; Returns:
; triple (i, j, i+j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(equal? (list 1 1 2) (make-pair-sum (list 1 1)))
(equal? (list 2 1 3) (make-pair-sum (list 2 1)))

; Filters element that satisfy specified predicate.
;
; Paramters:
; pred - predicate
; seq - sequence of elements
;
; Returns:
; filtered sequence of elements that satisfy predicate
(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; Enumerates pairs of unique numbers below n that sums to prime number.
;
; Parameters:
; n - maximum value for element of pair
;
; Returns:
; list of triples (i, j, i+j) that i+j is a prime
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(equal? (list (list 2 1 3) (list 3 2 5)) (prime-sum-pairs 3))
