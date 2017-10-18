#lang sicp

; Checks if specified items are the same.
;
; Items are expected to be symbols or list of symbols.
;
; Parameters:
; items1 - first list of items
; items2 - second list of items
;
; Returns:
; #t when two specified lists are the same, #f otherwise
(define (equal? item1 item2)
  (cond ((eq? item1 item2) #t)
        ((and (null? item1) (null? item2)) #t)
        ((and (pair? item1) (pair? item2))
         (and (equal? (car item1) (car item2))
              (equal? (cdr item1) (cdr item2))))
        (else #f)))

(equal? 'a 'a)
(not (equal? 'a 'b))
(equal? nil nil)
(equal? '(a) '(a))
(not (equal? '(a) '(b)))
(equal? '((a) b) '((a) b))
(not (equal? '((a) b) '((c) b)))