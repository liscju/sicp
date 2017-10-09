#lang sicp

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

; Calculates number of elements in the sequence.
;
; Parameters:
; sequence
;
; Returns:
; number of elements in the sequence
(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))


; To implement
; empty-board - zmienna
; safe? (k positions) czy hetman z k-tej kolumny innych nie szachuje
; adjoin-position (row column rest-of-queens)

; Empty board
(define empty-board (list))

(define (hetmans-in-diagonal k hetmans)
  (define hetmans-len (length hetmans))
  (if (null? hetmans)
      #f
      (or (or (= (+ (car hetmans) hetmans-len) k)
              (= (- (car hetmans) hetmans-len) k))
          (hetmans-in-diagonal k (cdr hetmans)))))


; Checks if inserting hetman in new column at position n
; is safe according to previous hetmans positions.
;
; Parameters:
; k - row of new hetman
; previous-hetman - rows of previous hetmans positions
;
; Returns:
; true if its safe to put new hetman in specified row
(define (safe? k previous-hetmans)
  (define previous-hetmans-in-different-rows
    (not (any (map (lambda (j) (= j k)) previous-hetmans))))
  (define previous-hetmans-not-in-diagonal
    (hetmans-in-diagonal k previous-hetmans))
  (and previous-hetmans-in-different-rows
       previous-hetmans-not-in-diagonal))

; Appends elements from first sequence to second sequence.
;
; Parameters:
; seq1 - first sequence
; seq2 - second sequence
;
; Parameters:
; sequence consisting of elements from first sequence added to
; second sequence
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (adjoin-position row column rest-of-queens)
  (append rest-of-queens (list column)))


; ustawiamy hetmanow kolumnami
; reprezentacja
; (list wiersz-kolumny-1 wiersz-kolumny-2 wiersz-kolumny-3)

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

; Generates all solutions to k queen puzzle.
;
; For information about eight queen puzzle see:
; https://en.wikipedia.org/wiki/Eight_queens_puzzle
;
; Parameters:
; board-size - size of the board
;
; Returns:
; solutions to eight queen puzzle 
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))












