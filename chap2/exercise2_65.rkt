#lang racket

; Gets value of root node of a tree.
;
; Parameters:
; tree - tree to get root node value
;
; Returns:
; value of rood node
(define (entry tree)
  (car tree))

; Gets left branch of a tree.
;
; Parameters:
; tree - tree to get left branch
;
; Returns:
; left branch
(define (left-branch tree)
  (cadr tree))

; Gets right branch of a tree.
;
; Parameters:
; tree - tree to get right branch
;
; Returns:
; right branch
(define (right-branch tree)
  (caddr tree))

; Constructs tree from entry, left and right branch.
;
; Parameters:
; entry - value of root node
; left - left branch
; right - right branch
;
; Returns:
; constructed tree
(define (make-tree entry left right)
  (list entry left right))

; Converts tree to list
;
; Parameters:
; tree - tree to convert
;
; Returns:
; list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                   (right-result (partial-tree (cdr non-left-elts)
                                               right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))

; Returns union of sets represented as ordered list.
;
; Parameters:
; s1, s2 - sets to union
;
; Returns:
; union of specified sets
(define (union-list-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (r1 (cdr s1))
               (x2 (car s2))
               (r2 (cdr s2)))
           (cond ((< x1 x2) (cons x1 (union-list-set r1 s2)))
                 ((> x1 x2) (cons x2 (union-list-set s1 r2)))
                 ((= x1 x2) (cons x1 (union-list-set r1 r2))))))))

; union-set, intersection-set with O(n)
; set represented as balanced tree

(define (union-set s1 s2)
  (list->tree (union-list-set (tree->list s1) (tree->list s2))))

(define (set-equal-list? s l)
  (equal? (tree->list s) l))

(set-equal-list? (union-set (list 5 '() '()) (list 10 '() '()))
                 (list 5 10))
(set-equal-list? (union-set (list 5 '() '()) (list 5 '() '()))
                 (list 5))
(set-equal-list? (union-set (list 5 '() (list 10 '() '()))
                            (list 10 (list 5 '() '()) '()))
                 (list 5 10))
(set-equal-list? (union-set (list 1 '() (list 4 '() '()))
                            (list 10 (list 5 '() '()) '()))
                 (list 1 4 5 10))
(set-equal-list? (union-set (list 1 '() (list 5 '() '()))
                            (list 10 (list 5 '() '()) '()))
                 (list 1 5 10))
(set-equal-list? (union-set (list 1 '() (list 5 '() (list 10 '() (list 15 '() '()))))
                            (list 10 (list 5 '() '()) '()))
                 (list 1 5 10 15))


(define (intersection-list-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-list-set (cdr set1)
                                            (cdr set2))))
              ((< x1 x2)
               (intersection-list-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-list-set set1 (cdr set2)))))))

(define (intersection-set s1 s2)
  (list->tree (intersection-list-set (tree->list s1) (tree->list s2))))

(set-equal-list? (intersection-set (list 5 '() '()) (list 10 '() '()))
                 (list))
(set-equal-list? (intersection-set (list 5 '() '()) (list 5 '() '()))
                 (list 5))
(set-equal-list? (intersection-set (list 5 '() (list 10 '() '()))
                                   (list 10 (list 5 '() '()) '()))
                 (list 5 10))
(set-equal-list? (intersection-set (list 1 '() (list 4 '() '()))
                                   (list 10 (list 5 '() '()) '()))
                 (list))
(set-equal-list? (intersection-set (list 1 '() (list 5 '() '()))
                                   (list 10 (list 5 '() '()) '()))
                 (list 5))
(set-equal-list? (intersection-set (list 1 '() (list 5 '() (list 10 '() (list 15 '() '()))))
                                   (list 10 (list 5 '() '()) '()))
                 (list 5 10))

